module Autofill exposing (AutofillData, Cache, CacheData, Msg, activeAssets, categories, codecStore, combined, empty, fetch, payees, update)

import Date exposing (Date)
import Http
import LunchMoney exposing (codecTransaction)
import Set
import Task
import Time
import TsJson.Codec as Codec exposing (Codec)
import Utils exposing (codecPosix, stringFromHttpError)
import Utils.Cached as Cached exposing (Cached)


type Cache
    = Cache CacheData


type alias CacheData =
    { categories : Cached String (List ( String, List LunchMoney.CategoryInfo ))
    , assets : Cached String (List LunchMoney.AssetInfo)
    , transactions : Cached String (List LunchMoney.Transaction)
    , lastUpdated : Maybe Time.Posix
    }


codecStore : Codec Cache
codecStore =
    Codec.object
        (\categories_ assets_ transcations_ lastUpdated_ ->
            Cache
                { categories = Cached.Available categories_
                , assets = Cached.Available assets_
                , transactions = Cached.Available transcations_
                , lastUpdated = lastUpdated_
                }
        )
        |> Codec.field "categories" (getFromStore .categories) (Codec.list (Codec.tuple Codec.string (Codec.list LunchMoney.codecCategoryInfo)))
        |> Codec.field "assets" (getFromStore .assets) (Codec.list LunchMoney.codecAssetInfo)
        |> Codec.field "transactions" (getFromStore .transactions) (Codec.list codecTransaction)
        |> Codec.maybeField "last_updated" (\(Cache cache) -> cache.lastUpdated) codecPosix
        |> Codec.buildObject


getFromStore : (CacheData -> Cached error (List a)) -> Cache -> List a
getFromStore getter (Cache cache) =
    getter cache
        |> remoteGetList


remoteGetList : Cached error (List a) -> List a
remoteGetList remote =
    remote
        |> Cached.withDefault []


type alias AutofillData =
    { lastUpdated : Time.Posix
    , info : AutofillInfo
    }


type AutofillInfo
    = AutofillInfo
        { categories : List ( String, List LunchMoney.CategoryInfo )
        , assets : List LunchMoney.AssetInfo
        , transactions : List LunchMoney.Transaction
        , payees : List String
        }


categories : AutofillInfo -> List ( String, List LunchMoney.CategoryInfo )
categories (AutofillInfo info) =
    info.categories


activeAssets : AutofillInfo -> List LunchMoney.AssetInfo
activeAssets (AutofillInfo info) =
    info.assets
        |> List.filter LunchMoney.assetIsActive


payees : AutofillInfo -> List String
payees (AutofillInfo info) =
    info.payees


empty : Cache
empty =
    Cache
        { categories = Cached.Missing
        , assets = Cached.Missing
        , transactions = Cached.Missing
        , lastUpdated = Nothing
        }


refresh : LunchMoney.Token -> Date -> (Msg -> msg) -> Cache -> Cmd msg
refresh token today toMsg cache =
    case combined cache of
        ( _, Just data ) ->
            let
                lastUpdatedDate =
                    Date.fromPosix Time.utc data.lastUpdated
            in
            if Date.diff Date.Days lastUpdatedDate today == 0 then
                Log.debug "Not refreshing autofill data, because it was already refreshed today."

            else
                fetch token today toMsg

        _ ->
            fetch token today toMsg


fetch : LunchMoney.Token -> Date -> (Msg -> msg) -> Cmd msg
fetch token today toMsg =
    let
        start =
            Date.add Date.Months -3 today

        end =
            today
    in
    Cmd.batch
        [ LunchMoney.getAllCategories token (GotCategories >> toMsg)
        , LunchMoney.getAllAssets token (GotAssets >> toMsg)
        , LunchMoney.getAllTransactions token
            { start = start, end = end }
            (GotTransactions >> toMsg)
        ]


type Msg
    = GotCategories (Result Http.Error LunchMoney.AllCategoriesResponse)
    | GotAssets (Result Http.Error LunchMoney.AllAssetsResponse)
    | GotTransactions (Result Http.Error LunchMoney.AllTransactionsResponse)
    | GotTime Time.Posix


update : Msg -> (Msg -> msg) -> Cache -> ( Cache, Cmd msg )
update msg toMsg (Cache model) =
    case msg of
        GotCategories result ->
            ( Cache
                { model
                    | categories =
                        result
                            |> Result.mapError stringFromHttpError
                            |> Result.map LunchMoney.groupEntries
                            |> Cached.updateWithResult model.categories
                }
            , updateTime result |> Cmd.map toMsg
            )

        GotAssets result ->
            ( Cache
                { model
                    | assets =
                        result
                            |> Result.mapError stringFromHttpError
                            |> Cached.updateWithResult model.assets
                }
            , updateTime result |> Cmd.map toMsg
            )

        GotTransactions result ->
            ( Cache
                { model
                    | transactions =
                        result
                            |> Result.mapError stringFromHttpError
                            |> Cached.updateWithResult model.transactions
                }
            , updateTime result |> Cmd.map toMsg
            )

        GotTime now ->
            ( Cache { model | lastUpdated = Just now }, Cmd.none )


updateTime : Result e o -> Cmd Msg
updateTime result =
    case result of
        Ok _ ->
            Time.now |> Task.perform GotTime

        Err _ ->
            Cmd.none


combined : Cache -> ( Maybe String, Maybe AutofillData )
combined (Cache remote) =
    Cached.combine3
        (\categories_ assets_ transactions_ ->
            AutofillInfo
                { categories = categories_
                , assets = assets_
                , transactions = transactions_
                , payees =
                    transactions_
                        |> List.filterMap
                            (\transaction ->
                                transaction.payee
                                    |> Maybe.andThen
                                        (\payee ->
                                            case payee of
                                                "" ->
                                                    Nothing

                                                "[No Payee]" ->
                                                    Nothing

                                                p ->
                                                    Just p
                                        )
                            )
                        |> Set.fromList
                        |> Set.toList
                }
        )
        remote.categories
        remote.assets
        remote.transactions
        |> Tuple.mapSecond
            (\maybeInfo ->
                Maybe.map2
                    (\lastUpdated info ->
                        { lastUpdated = lastUpdated
                        , info = info
                        }
                    )
                    remote.lastUpdated
                    maybeInfo
            )
