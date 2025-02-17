module LunchMoneyInfo exposing (LunchMoneyInfo, Msg, Store, StoreData, activeAssets, categories, codecStore, combined, empty, fetch, payees, update)

import Date exposing (Date)
import Http
import LunchMoney exposing (codecTransaction)
import RemoteData exposing (RemoteData)
import Set
import TsJson.Codec as Codec exposing (Codec)
import Utils exposing (stringFromHttpError)


type Store
    = Store StoreData


type alias StoreData =
    { categories : RemoteData String (List ( String, List LunchMoney.CategoryInfo ))
    , assets : RemoteData String (List LunchMoney.AssetInfo)
    , transactions : RemoteData String (List LunchMoney.Transaction)
    }


codecStore : Codec Store
codecStore =
    Codec.object
        (\categories_ assets_ transcations_ ->
            Store
                { categories = RemoteData.Success categories_
                , assets = RemoteData.Success assets_
                , transactions = RemoteData.Success transcations_
                }
        )
        |> Codec.field "categories" (getFromStore .categories) (Codec.list (Codec.tuple Codec.string (Codec.list LunchMoney.codecCategoryInfo)))
        |> Codec.field "assets" (getFromStore .assets) (Codec.list LunchMoney.codecAssetInfo)
        |> Codec.field "transactions" (getFromStore .transactions) (Codec.list codecTransaction)
        |> Codec.buildObject


getFromStore : (StoreData -> RemoteData error (List a)) -> Store -> List a
getFromStore getter (Store store) =
    getter store
        |> remoteGetList


remoteGetList : RemoteData error (List a) -> List a
remoteGetList remote =
    remote
        |> RemoteData.withDefault []


type LunchMoneyInfo
    = LunchMoneyInfo
        { categories : List ( String, List LunchMoney.CategoryInfo )
        , assets : List LunchMoney.AssetInfo
        , transactions : List LunchMoney.Transaction
        , payees : List String
        }


categories : LunchMoneyInfo -> List ( String, List LunchMoney.CategoryInfo )
categories (LunchMoneyInfo info) =
    info.categories


activeAssets : LunchMoneyInfo -> List LunchMoney.AssetInfo
activeAssets (LunchMoneyInfo info) =
    info.assets
        |> List.filter LunchMoney.assetIsActive


payees : LunchMoneyInfo -> List String
payees (LunchMoneyInfo info) =
    info.payees


empty : Store
empty =
    Store
        { categories = RemoteData.NotAsked
        , assets = RemoteData.NotAsked
        , transactions = RemoteData.NotAsked
        }


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


update : Msg -> Store -> ( Store, Cmd msg )
update msg (Store model) =
    case msg of
        GotCategories result ->
            ( Store
                { model
                    | categories =
                        result
                            |> Result.mapError stringFromHttpError
                            |> Result.map LunchMoney.groupEntries
                            |> RemoteData.fromResult
                }
            , Cmd.none
            )

        GotAssets result ->
            ( Store
                { model
                    | assets =
                        result
                            |> Result.mapError stringFromHttpError
                            |> RemoteData.fromResult
                }
            , Cmd.none
            )

        GotTransactions result ->
            ( Store
                { model
                    | transactions =
                        result
                            |> Result.mapError stringFromHttpError
                            |> RemoteData.fromResult
                }
            , Cmd.none
            )


combined : Store -> RemoteData String LunchMoneyInfo
combined (Store remote) =
    RemoteData.map3
        (\categories_ assets_ transactions_ ->
            LunchMoneyInfo
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
