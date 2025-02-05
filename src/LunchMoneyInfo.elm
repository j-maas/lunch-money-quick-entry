module LunchMoneyInfo exposing (LunchMoneyInfo, Msg, Remote, combined, empty, fetch, update)

import Http
import LunchMoney
import RemoteData exposing (RemoteData)
import Utils exposing (stringFromHttpError)


type alias Remote =
    { categories : RemoteData String (List LunchMoney.CategoryInfo)
    , assets : RemoteData String (List LunchMoney.AssetInfo)
    }


type alias LunchMoneyInfo =
    { categories : List LunchMoney.CategoryInfo
    , assets : List LunchMoney.AssetInfo
    }


empty : Remote
empty =
    { categories = RemoteData.NotAsked
    , assets = RemoteData.NotAsked
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


update : Msg -> Remote -> ( Remote, Cmd msg )
update msg model =
    case msg of
        GotCategories result ->
            ( { model
                | categories =
                    result
                        |> Result.mapError stringFromHttpError
                        |> Result.map LunchMoney.flattenEntries
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )

        GotAssets result ->
            ( { model
                | assets =
                    result
                        |> Result.mapError stringFromHttpError
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )


combined : Remote -> RemoteData String LunchMoneyInfo
combined remote =
    RemoteData.map2 LunchMoneyInfo
        remote.categories
        remote.assets
