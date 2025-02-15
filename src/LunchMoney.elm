module LunchMoney exposing (AllAssetsResponse, AllCategoriesResponse, Amount, AssetInfo, CategoryEntry(..), CategoryInfo, InsertResponse, Token, Transaction, amountFromCents, amountToString, assetId, assetIsActive, assetName, codecAmount, codecTransaction, flattenEntries, getAllAssets, getAllCategories, groupEntries, insertTransactions, tokenFromString, tokenToString)

import Date exposing (Date)
import Http
import Json.Encode as Json
import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as Decode exposing (Decoder)
import TsJson.Encode as Encode
import Utils exposing (codecDate)


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
    , assetId : Maybe Int
    }


codecTransaction : Codec Transaction
codecTransaction =
    Codec.object Transaction
        |> Codec.field "date" .date codecDate
        |> Codec.field "amount" .amount codecAmount
        |> Codec.maybeField "category_id" .categoryId Codec.int
        |> Codec.maybeField "asset_id" .assetId Codec.int
        |> Codec.buildObject


codecAmount : Codec Amount
codecAmount =
    Codec.build (Encode.string |> Encode.map amountToString)
        (Decode.string
            |> Decode.andThen
                (Decode.andThenInit
                    (\raw ->
                        case String.split "." raw of
                            [ rawBefore, rawAfter ] ->
                                case ( String.toInt rawBefore, String.toInt rawAfter ) of
                                    ( Just before, Just after ) ->
                                        let
                                            cents =
                                                before * 100 + after
                                        in
                                        amountFromCents cents
                                            |> Decode.succeed

                                    _ ->
                                        Decode.fail ("Expected integers, got " ++ rawBefore ++ "." ++ rawAfter)

                            _ ->
                                Decode.fail ("Expected exactly one decimal point, but got '" ++ raw ++ "'")
                    )
                )
        )


insertTransactions : Token -> List Transaction -> (Result Http.Error InsertResponse -> msg) -> Cmd msg
insertTransactions token transactions toMsg =
    post token
        [ "v1", "transactions" ]
        (transactions
            |> Encode.encoder
                (Encode.object
                    [ Encode.required "transactions" identity (Encode.list (Codec.encoder codecTransaction))
                    ]
                )
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


groupEntries : List CategoryEntry -> List ( String, List CategoryInfo )
groupEntries entries =
    entries
        |> List.map
            (\entry ->
                case entry of
                    CategoryGroupEntry group ->
                        ( group.info.name, group.children )

                    CategoryEntry info ->
                        ( "", [ info ] )
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
            (Decode.andThenInit
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


type AssetInfo
    = AssetInfo
        { id : Int
        , name : String
        , displayName : Maybe String
        , excludeTransactions : Bool
        , closedOn : Maybe Date
        }


assetId : AssetInfo -> Int
assetId (AssetInfo info) =
    info.id


assetName : AssetInfo -> String
assetName (AssetInfo info) =
    case info.displayName of
        Just displayName ->
            displayName

        Nothing ->
            info.name


assetIsActive : AssetInfo -> Bool
assetIsActive (AssetInfo info) =
    case info.closedOn of
        Just _ ->
            False

        Nothing ->
            not info.excludeTransactions


decodeAllAssetsResponse : Decoder AllAssetsResponse
decodeAllAssetsResponse =
    Decode.field "assets" (Decode.list decodeAssetInfo)


decodeAssetInfo : Decoder AssetInfo
decodeAssetInfo =
    Decode.map5
        (\id name displayName excludeTransactions closedOn ->
            AssetInfo
                { id = id
                , name = name
                , displayName = displayName
                , excludeTransactions = excludeTransactions
                , closedOn = closedOn
                }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "display_name" (Decode.maybe Decode.string))
        (Decode.field "exclude_transactions" Decode.bool)
        (Decode.field "closed_on" (Decode.maybe decodeDate))


decodeDate : Decoder Date
decodeDate =
    Decode.string
        |> Decode.andThen
            (Decode.andThenInit
                (\raw ->
                    case Date.fromIsoString raw of
                        Ok date ->
                            Decode.succeed date

                        Err err ->
                            Decode.fail err
                )
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
        , expect = Http.expectJson toMsg (Decode.decoder decodeResponse)
        , timeout = timeout
        , tracker = Nothing
        }


post : Token -> List String -> Json.Value -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
post token path body decodeResponse toMsg =
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = baseDomain ++ String.join "/" path
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg (Decode.decoder decodeResponse)
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
