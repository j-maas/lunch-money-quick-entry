module LunchMoney exposing (AllAssetsResponse, AllCategoriesResponse, AllTransactionsResponse, Amount, AssetInfo, CategoryEntry(..), CategoryInfo, InsertResponse, Token, Transaction, amountFromCents, amountToString, assetId, assetIsActive, assetName, codecAmount, codecAssetInfo, codecCategoryInfo, codecTransaction, flattenEntries, getAllAssets, getAllCategories, getAllTransactions, groupEntries, insertTransactions, tokenFromString, tokenToString)

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
    , payee : Maybe String
    , categoryId : Maybe Int
    , assetId : Maybe Int
    }


codecTransaction : Codec Transaction
codecTransaction =
    Codec.object Transaction
        |> Codec.field "date" .date codecDate
        |> Codec.field "amount" .amount codecAmount
        |> Codec.maybeField "payee"
            (\transaction ->
                case transaction.payee of
                    Nothing ->
                        Just "[No Payee]"

                    Just payee ->
                        Just payee
            )
            Codec.string
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
                    [ Encode.required "transactions"
                        identity
                        (Encode.list (Codec.encoder codecTransaction))
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


getAllTransactions : Token -> { start : Date, end : Date } -> (Result Http.Error AllTransactionsResponse -> msg) -> Cmd msg
getAllTransactions token range toMsg =
    get token
        [ "v1", "transactions" ]
        [ ( "start_date", Date.toIsoString range.start )
        , ( "end_date", Date.toIsoString range.end )
        ]
        decodeAllTransactionsResponse
        toMsg


type alias AllTransactionsResponse =
    List Transaction


decodeAllTransactionsResponse : Decoder AllTransactionsResponse
decodeAllTransactionsResponse =
    Decode.field "transactions" (Decode.list (Codec.decoder codecTransaction))


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


codecCategoryInfo : Codec CategoryInfo
codecCategoryInfo =
    Codec.object CategoryInfo
        |> Codec.field "id" .id Codec.int
        |> Codec.field "name" .name Codec.string
        |> Codec.field "archived" .archived Codec.bool
        |> Codec.buildObject


decodeCategoryInfo : Decoder CategoryInfo
decodeCategoryInfo =
    Codec.decoder codecCategoryInfo


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


codecAssetInfo : Codec AssetInfo
codecAssetInfo =
    Codec.object
        (\id name displayName excludeTransactions closedOn ->
            AssetInfo
                { id = id
                , name = name
                , displayName = displayName
                , excludeTransactions = excludeTransactions
                , closedOn = closedOn
                }
        )
        |> Codec.field "id" (\(AssetInfo info) -> info.id) Codec.int
        |> Codec.field "name" (\(AssetInfo info) -> info.name) Codec.string
        |> Codec.maybeField "display_name" (\(AssetInfo info) -> info.displayName) Codec.string
        |> Codec.field "exclude_transactions" (\(AssetInfo info) -> info.excludeTransactions) Codec.bool
        |> Codec.maybeField "closed_on" (\(AssetInfo info) -> info.closedOn) codecDate
        |> Codec.buildObject


decodeAssetInfo : Decoder AssetInfo
decodeAssetInfo =
    Codec.decoder codecAssetInfo


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
    Just 5000
