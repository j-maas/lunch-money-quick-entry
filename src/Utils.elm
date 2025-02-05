module Utils exposing (stringFromHttpError)

import Http


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
            "I sent a malformed body: " ++ bodyError
