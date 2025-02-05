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
    , categories : List LunchMoney.CategoryInfo
    , categoryInput : String
    , amountInput : String
    , insertState : RemoteData Http.Error LunchMoney.InsertResponse
    , error : Maybe String
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsRaw =
    case InteropPorts.decodeFlags flagsRaw of
        Err flagsError ->
            ( Err <| Decode.errorToString flagsError
            , Cmd.none
            )

        Ok flags ->
            let
                ( maybeError, maybeCategoryCmd ) =
                    case flags.maybeToken of
                        Just token ->
                            ( Nothing, LunchMoney.getAllCategories token GotCategories )

                        Nothing ->
                            ( Just "No token given at startup, not getting categories.", Cmd.none )
            in
            ( Ok
                { token = flags.maybeToken
                , dateInput = flags.today
                , categories = []
                , categoryInput = ""
                , amountInput = ""
                , insertState = RemoteData.NotAsked
                , error = maybeError
                }
            , maybeCategoryCmd
            )


type Msg
    = ChangedToken String
    | ChangedDateInput String
    | ChangedAmountInput String
    | ChangedCategoryInput String
    | TappedInsertTransaction
    | GotInsertedTransactions (Result Http.Error LunchMoney.InsertResponse)
    | GotCategories (Result Http.Error LunchMoney.AllCategoriesResponse)


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

        ChangedCategoryInput newCategory ->
            ( { model | categoryInput = newCategory }, Cmd.none )

        TappedInsertTransaction ->
            case ( model.token, Date.fromIsoString model.dateInput ) of
                ( Just token, Ok date ) ->
                    let
                        matchingCategories =
                            model.categories
                                |> List.filter (\category -> category.name == model.categoryInput)

                        maybeCategoryId =
                            case matchingCategories of
                                [ match ] ->
                                    Just match.id

                                _ ->
                                    Nothing

                        transaction =
                            { date = date
                            , amount =
                                model.amountInput
                                    |> String.toInt
                                    |> Maybe.withDefault 123
                                    |> LunchMoney.amountFromCents
                            , categoryId = maybeCategoryId
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

        GotCategories result ->
            case result of
                Ok categories ->
                    let
                        newCategories =
                            categories
                                |> List.concatMap
                                    (\entry ->
                                        case entry of
                                            LunchMoney.CategoryEntry info ->
                                                [ info ]

                                            LunchMoney.CategoryGroupEntry e ->
                                                e.children
                                    )
                    in
                    ( { model | categories = newCategories }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = Just <| "Error getting categories: " ++ stringFromHttpError err }, Cmd.none )


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
                    [ Html.text ("An error occurred: " ++ stringFromHttpError error) ]

                RemoteData.Success _ ->
                    [ Html.text "Successfully inserted" ]

        errorDisplay =
            case model.error of
                Just err ->
                    [ Html.text ("Error: " ++ err) ]

                Nothing ->
                    []
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
                , Css.alignItems Css.stretch
                , Css.gap (Css.rem 1)
                ]
            , Event.onSubmit TappedInsertTransaction
            ]
            ([ [ textInput
                    (case model.token of
                        Just token ->
                            LunchMoney.tokenToString token

                        Nothing ->
                            ""
                    )
                    ChangedToken
                    []
               ]
                |> labeled "Access token" []
             , [ dateInput model.dateInput ChangedDateInput ]
                |> labeled "Date" []
             , [ textInput model.amountInput ChangedAmountInput [ Attr.attribute "inputmode" "numeric" ] ]
                |> labeled "Amount" []
             , autocompleteInput "categoryList"
                model.categoryInput
                ChangedCategoryInput
                (model.categories |> List.map .name)
                |> labeled "Category" [ Css.width (Css.pct 100) ]
             , Html.button
                [ Attr.type_ "submit"
                , Attr.css [ Css.alignSelf Css.start ]
                ]
                [ Html.text "Insert" ]
             ]
                ++ insertionIndicator
                ++ errorDisplay
            )
        ]


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


textInput : String -> (String -> msg) -> List (Html.Attribute msg) -> Html msg
textInput value toMsg attributes =
    Html.input
        ([ Attr.type_ "text"
         , Attr.value value
         , Event.onInput toMsg
         ]
            ++ attributes
        )
        []


dateInput : String -> (String -> msg) -> Html msg
dateInput value toMsg =
    Html.input
        [ Attr.type_ "date"
        , Attr.value value
        , Event.onInput toMsg
        ]
        []


autocompleteInput : String -> String -> (String -> msg) -> List String -> List (Html msg)
autocompleteInput listId current toMsg options =
    [ textInput current toMsg [ Attr.list listId, Event.onInput toMsg ]
    , Html.datalist [ Attr.id listId ]
        (options
            |> List.map
                (\option ->
                    Html.option [ Attr.value option ] []
                )
        )
    ]


labeled : String -> List Css.Style -> List (Html msg) -> Html msg
labeled label styles children =
    Html.label
        [ Attr.css
            ([ Css.display Css.flex_
             , Css.flexDirection Css.column
             ]
                ++ styles
            )
        ]
        (Html.text label
            :: children
        )


loaderView : Html Msg
loaderView =
    Html.text "Loading..."
