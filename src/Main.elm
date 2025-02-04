module Main exposing (main)

import Browser
import Css
import Date
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Http
import InteropDefinitions
import InteropPorts
import Json.Decode as Decode
import LunchMoney
import RemoteData exposing (RemoteData)


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Result String AppModel


type alias AppModel =
    { token : Maybe LunchMoney.Token
    , dateInput : String
    , amountInput : String
    , insertState : RemoteData Http.Error LunchMoney.InsertResponse
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsRaw =
    case InteropPorts.decodeFlags flagsRaw of
        Err flagsError ->
            ( Err <| Decode.errorToString flagsError
            , Cmd.none
            )

        Ok flags ->
            ( Ok
                { token = flags.maybeToken
                , dateInput = flags.today
                , amountInput = ""
                , insertState = RemoteData.NotAsked
                }
            , Cmd.none
            )


type Msg
    = ChangedToken String
    | ChangedDateInput String
    | ChangedAmountInput String
    | TappedInsertTransaction
    | GotInsertedTransactions (Result Http.Error LunchMoney.InsertResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Err _ ->
            ( model, Cmd.none )

        Ok appModel ->
            updateAppModel msg appModel
                |> Tuple.mapFirst Ok


updateAppModel : Msg -> AppModel -> ( AppModel, Cmd Msg )
updateAppModel msg model =
    case msg of
        ChangedToken newTokenRaw ->
            let
                newToken =
                    LunchMoney.tokenFromString newTokenRaw

                maybeStoreToken =
                    case newToken of
                        Just token ->
                            storeToken token

                        Nothing ->
                            Cmd.none
            in
            ( { model | token = newToken }
            , maybeStoreToken
            )

        ChangedDateInput newDate ->
            ( { model | dateInput = newDate }, Cmd.none )

        ChangedAmountInput newAmount ->
            ( { model | amountInput = newAmount }, Cmd.none )

        TappedInsertTransaction ->
            case ( model.token, Date.fromIsoString model.dateInput ) of
                ( Just token, Ok date ) ->
                    let
                        transaction =
                            { date = date
                            , amount =
                                model.amountInput
                                    |> String.toInt
                                    |> Maybe.withDefault 123
                                    |> LunchMoney.amountFromCents
                            }

                        insertTransactionsCmd =
                            LunchMoney.insertTransactions
                                token
                                [ transaction ]
                                GotInsertedTransactions
                    in
                    ( { model
                        | insertState = RemoteData.Loading
                      }
                    , insertTransactionsCmd
                    )

                _ ->
                    ( model, Cmd.none )

        GotInsertedTransactions result ->
            ( { model
                | amountInput = ""
                , insertState = result |> RemoteData.fromResult
              }
            , Cmd.none
            )


tokenSettingKey : String
tokenSettingKey =
    "token"


storeToken : LunchMoney.Token -> Cmd Msg
storeToken token =
    InteropDefinitions.StoreSetting
        { key = tokenSettingKey
        , value = LunchMoney.tokenToString token
        }
        |> InteropPorts.fromElm


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


view : Model -> Html Msg
view modelResult =
    case modelResult of
        Err error ->
            Html.text error

        Ok appModel ->
            appView appModel


appView : AppModel -> Html Msg
appView model =
    let
        insertionIndicator =
            case model.insertState of
                RemoteData.NotAsked ->
                    []

                RemoteData.Loading ->
                    [ loaderView
                    ]

                RemoteData.Failure error ->
                    let
                        errorDisplay =
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
                    in
                    [ Html.text ("An error occurred: " ++ errorDisplay) ]

                RemoteData.Success _ ->
                    [ Html.text "Successfully inserted" ]
    in
    Html.main_
        [ Attr.css
            [ Css.display Css.flex_
            , Css.alignItems Css.center
            , Css.height (Css.pct 100)
            , Css.margin (Css.rem 1)
            ]
        ]
        [ Html.form
            [ Attr.css
                [ Css.display Css.flex_
                , Css.flexDirection Css.column
                , Css.alignItems Css.start
                , Css.gap (Css.rem 1)
                ]
            , Event.onSubmit TappedInsertTransaction
            ]
            ([ textInput
                (case model.token of
                    Just token ->
                        LunchMoney.tokenToString token

                    Nothing ->
                        ""
                )
                ChangedToken
                |> labeled "Access token" [ Css.width (Css.pct 100) ]
             , dateInput model.dateInput ChangedDateInput
                |> labeled "Date" [ Css.width (Css.pct 100) ]
             , textInput model.amountInput ChangedAmountInput
                |> labeled "Amount" [ Css.width (Css.pct 100) ]
             , Html.button [ Attr.type_ "submit" ] [ Html.text "Insert" ]
             ]
                ++ insertionIndicator
            )
        ]


textInput : String -> (String -> msg) -> Html msg
textInput value toMsg =
    Html.input
        [ Attr.type_ "text"
        , Attr.value value
        , Event.onInput toMsg
        ]
        []


dateInput : String -> (String -> msg) -> Html msg
dateInput value toMsg =
    Html.input
        [ Attr.type_ "date"
        , Attr.value value
        , Event.onInput toMsg
        ]
        []


labeled : String -> List Css.Style -> Html msg -> Html msg
labeled label styles child =
    Html.label
        [ Attr.css
            ([ Css.display Css.flex_
             , Css.flexDirection Css.column
             ]
                ++ styles
            )
        ]
        [ Html.text label
        , child
        ]


loaderView : Html Msg
loaderView =
    Html.text "Loading..."
