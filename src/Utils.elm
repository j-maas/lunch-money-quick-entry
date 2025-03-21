module Utils exposing (codecDate, codecPosix, stringFromHttpError)

import Date exposing (Date)
import Http
import Time
import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as Decode
import TsJson.Encode as Encode


stringFromHttpError : Http.Error -> String
stringFromHttpError error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " is not valid."

        Http.NetworkError ->
            "Please check your internet connection."

        Http.Timeout ->
            "The server is taking a long time to respond."

        Http.BadStatus status ->
            "The server responded with a bad status of " ++ String.fromInt status ++ "."

        Http.BadBody bodyError ->
            "I got a malformed body: " ++ bodyError


codecDate : Codec Date
codecDate =
    Codec.build
        (Encode.string
            |> Encode.map Date.toIsoString
        )
        (Decode.string
            |> Decode.andThen
                (Decode.andThenInit
                    (\rawDate ->
                        case Date.fromIsoString rawDate of
                            Ok date ->
                                Decode.succeed date

                            Err err ->
                                Decode.fail err
                    )
                )
        )


codecPosix : Codec Time.Posix
codecPosix =
    Codec.int
        |> Codec.map Time.millisToPosix Time.posixToMillis
