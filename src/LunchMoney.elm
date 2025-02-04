module LunchMoney exposing (Amount, InsertResponse, Token, Transaction, amountFromCents, amountToString, insertTransactions, tokenFromString, tokenToString)

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
    }


encodeTransaction : Transaction -> Encode.Value
encodeTransaction transaction =
    Encode.object
        [ ( "date", transaction.date |> Date.toIsoString |> Encode.string )
        , ( "amount", transaction.amount |> amountToString |> Encode.string )
        ]


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
