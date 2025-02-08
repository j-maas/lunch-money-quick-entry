module LunchMoney exposing (AllAssetsResponse, AllCategoriesResponse, Amount, AssetInfo, CategoryEntry(..), CategoryInfo, InsertResponse, Token, Transaction, amountFromCents, amountToString, assetName, flattenEntries, getAllAssets, getAllCategories, insertTransactions, tokenFromString, tokenToString, showAssetSelection)

import Date exposing (Date)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Token
    = Token String


tokenFromString : String -> Maybe Token
tokenFromString raw =
    if String.isEmpty raw then
        Nothing

    else
        Token raw |> Just


tokenToString : Token -> String
tokenToString (Token raw) =
    raw


type Amount
    = Cents Int


amountFromCents : Int -> Amount
amountFromCents cents =
    Cents cents


amountToString : Amount -> String
amountToString (Cents cents) =
    let
        centsString =
            cents |> String.fromInt

        euros =
            centsString |> String.dropRight 2

        remainingCents =
            centsString |> String.right 2
    in
    euros ++ "." ++ remainingCents


type alias Transaction =
    { date : Date
    , amount : Amount
    , categoryId : Maybe Int
    }


encodeTransaction : Transaction -> Encode.Value
encodeTransaction transaction =
    let
        maybeCategoryId =
            case transaction.categoryId of
                Just categoryId ->
                    [ ( "category_id", categoryId |> Encode.int ) ]

                Nothing ->
                    []
    in
    Encode.object
        ([ ( "date", transaction.date |> Date.toIsoString |> Encode.string )
         , ( "amount", transaction.amount |> amountToString |> Encode.string )
         ]
            ++ maybeCategoryId
        )


insertTransactions : Token -> List Transaction -> (Result Http.Error InsertResponse -> msg) -> Cmd msg
insertTransactions token transactions toMsg =
    post token
        [ "v1", "transactions" ]
        (Encode.object
            [ ( "transactions", Encode.list encodeTransaction transactions )
            ]
        )
        decodeInsertResponse
        toMsg


type alias InsertResponse =
    List Int


decodeInsertResponse : Decoder InsertResponse
decodeInsertResponse =
    Decode.field "ids" (Decode.list Decode.int)


getAllCategories : Token -> (Result Http.Error AllCategoriesResponse -> msg) -> Cmd msg
getAllCategories token toMsg =
    get token
        [ "v1", "categories" ]
        [ ( "format", "nested" ) ]
        decodeAllCategoriesResponse
        toMsg


type alias AllCategoriesResponse =
    List CategoryEntry


type CategoryEntry
    = CategoryEntry CategoryInfo
    | CategoryGroupEntry CategoryGroup


type alias CategoryInfo =
    { id : Int
    , name : String
    , archived : Bool
    }


type alias CategoryGroup =
    { info : CategoryInfo
    , children : List CategoryInfo
    }


flattenEntries : List CategoryEntry -> List CategoryInfo
flattenEntries entries =
    entries
        |> List.concatMap
            (\entry ->
                case entry of
                    CategoryEntry info ->
                        [ info ]

                    CategoryGroupEntry e ->
                        e.children
            )


decodeAllCategoriesResponse : Decoder (List CategoryEntry)
decodeAllCategoriesResponse =
    Decode.field "categories" decodeNestedCategories


decodeNestedCategories : Decoder (List CategoryEntry)
decodeNestedCategories =
    Decode.map2
        (\info isGroup ->
            { info = info
            , isGroup = isGroup
            }
        )
        decodeCategoryInfo
        (Decode.field "is_group" Decode.bool)
        |> Decode.andThen
            (\entry ->
                if entry.isGroup then
                    Decode.field "children" (Decode.list decodeCategoryInfo)
                        |> Decode.map
                            (\children ->
                                CategoryGroupEntry
                                    { info = entry.info
                                    , children = children
                                    }
                            )

                else
                    Decode.succeed (CategoryEntry entry.info)
            )
        |> Decode.list


decodeCategoryInfo : Decoder CategoryInfo
decodeCategoryInfo =
    Decode.map3 CategoryInfo
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "archived" Decode.bool)


getAllAssets : Token -> (Result Http.Error AllAssetsResponse -> msg) -> Cmd msg
getAllAssets token toMsg =
    get token
        [ "v1", "assets" ]
        []
        decodeAllAssetsResponse
        toMsg


type alias AllAssetsResponse =
    List AssetInfo


type alias AssetInfo =
    { id : Int
    , name : String
    , displayName : Maybe String
    , excludeTransactions : Bool
    , closedOn : Maybe Date
    }


showAssetSelection : List AssetInfo -> List String
showAssetSelection all =
    all
        |> List.filter
            (\info ->
                case info.closedOn of
                    Just _ ->
                        False

                    Nothing ->
                        not info.excludeTransactions
            )
        |> List.map assetName


assetName : AssetInfo -> String
assetName info =
    case info.displayName of
        Just displayName ->
            displayName

        Nothing ->
            info.name


decodeAllAssetsResponse : Decoder AllAssetsResponse
decodeAllAssetsResponse =
    Decode.field "assets" (Decode.list decodeAssetInfo)


decodeAssetInfo : Decoder AssetInfo
decodeAssetInfo =
    Decode.map5 AssetInfo
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "display_name" (Decode.maybe Decode.string))
        (Decode.field "exclude_transactions" Decode.bool)
        (Decode.field "closed_on" (Decode.maybe decodeDate))


decodeDate : Decoder Date
decodeDate =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case Date.fromIsoString raw of
                    Ok date ->
                        Decode.succeed date

                    Err err ->
                        Decode.fail err
            )


get : Token -> List String -> List ( String, String ) -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
get token path queryParams decodeResponse toMsg =
    let
        queryParamsEncoded =
            queryParams
                |> List.map (\( key, value ) -> key ++ "=" ++ value)
                |> String.join "&"
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = baseDomain ++ String.join "/" path ++ "?" ++ queryParamsEncoded
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeResponse
        , timeout = timeout
        , tracker = Nothing
        }


post : Token -> List String -> Encode.Value -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
post token path body decodeResponse toMsg =
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = baseDomain ++ String.join "/" path
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg decodeResponse
        , timeout = timeout
        , tracker = Nothing
        }


baseDomain : String
baseDomain =
    "https://dev.lunchmoney.app/"


authHeader : Token -> Http.Header
authHeader (Token token) =
    Http.header "Authorization" ("Bearer " ++ token)


timeout : Maybe Float
timeout =
    Just 10000
