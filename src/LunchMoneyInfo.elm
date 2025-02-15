module LunchMoneyInfo exposing (LunchMoneyInfo, Msg, Store, activeAssets, categories, combined, empty, fetch, payees, update)

import Http
import LunchMoney
import RemoteData exposing (RemoteData)
import Utils exposing (stringFromHttpError)


type Store
    = Store
        { categories : RemoteData String (List ( String, List LunchMoney.CategoryInfo ))
        , assets : RemoteData String (List LunchMoney.AssetInfo)
        , payees : List String
        }


type LunchMoneyInfo
    = LunchMoneyInfo
        { categories : List ( String, List LunchMoney.CategoryInfo )
        , assets : List LunchMoney.AssetInfo
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
        , payees = []
        }


fetch : LunchMoney.Token -> (Msg -> msg) -> Cmd msg
fetch token toMsg =
    Cmd.batch
        [ LunchMoney.getAllCategories token (GotCategories >> toMsg)
        , LunchMoney.getAllAssets token (GotAssets >> toMsg)
        ]


type Msg
    = GotCategories (Result Http.Error LunchMoney.AllCategoriesResponse)
    | GotAssets (Result Http.Error LunchMoney.AllAssetsResponse)


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


combined : Store -> RemoteData String LunchMoneyInfo
combined (Store remote) =
    RemoteData.map2
        (\categories_ assets_ ->
            LunchMoneyInfo
                { categories = categories_
                , assets = assets_
                , payees = remote.payees
                }
        )
        remote.categories
        remote.assets
