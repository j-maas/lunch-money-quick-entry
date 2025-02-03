module Main exposing (main)

import Browser
import Html exposing (Html, div)


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view _ =
    div []
        [ Html.text "Hello from Elm."
        ]
