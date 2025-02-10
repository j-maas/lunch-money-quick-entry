module Main exposing (main)

import Browser
import Css
import Date
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import InsertQueue exposing (InsertQueue)
import InteropDefinitions
import InteropPorts
import Json.Decode as Decode
import LunchMoney
import LunchMoneyInfo
import RemoteData
import TsJson.Codec as Codec
import TsJson.Encode as Encode


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
    , lunchMoneyInfo : LunchMoneyInfo.Store
    , categoryInput : String
    , selectedAsset : String
    , amountInput : String
    , insertQueue : InsertQueue
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
                ( maybeError, maybeLunchMoneyInfoCmd ) =
                    case flags.maybeToken of
                        Just token ->
                            ( Nothing, LunchMoneyInfo.fetch token GotLunchMoneyInfoMsg )

                        Nothing ->
                            ( Just "No token given at startup, not getting updates.", Cmd.none )
            in
            ( Ok
                { token = flags.maybeToken
                , dateInput = flags.today
                , lunchMoneyInfo = LunchMoneyInfo.empty
                , amountInput = ""
                , categoryInput = ""
                , selectedAsset = ""
                , insertQueue = InsertQueue.empty
                , error = maybeError
                }
            , maybeLunchMoneyInfoCmd
            )


type Msg
    = GotLunchMoneyInfoMsg LunchMoneyInfo.Msg
    | ChangedToken String
    | ChangedDateInput String
    | ChangedAmountInput String
    | ChangedCategoryInput String
    | ChangedAssetInput String
    | TappedInsertTransaction
    | GotInsertQueueMsg InsertQueue.Msg


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
        GotLunchMoneyInfoMsg m ->
            let
                ( newLunchMoneyInfo, cmd ) =
                    LunchMoneyInfo.update m model.lunchMoneyInfo

                selectedAsset =
                    case newLunchMoneyInfo |> LunchMoneyInfo.combined of
                        RemoteData.Success info ->
                            case LunchMoneyInfo.activeAssets info of
                                first :: _ ->
                                    LunchMoney.assetId first |> String.fromInt

                                _ ->
                                    ""

                        _ ->
                            ""
            in
            ( { model
                | lunchMoneyInfo = newLunchMoneyInfo
                , selectedAsset = selectedAsset
              }
            , cmd
            )

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

        ChangedAssetInput newAssetId ->
            ( { model | selectedAsset = newAssetId }, Cmd.none )

        TappedInsertTransaction ->
            let
                token_ =
                    model.token

                date_ =
                    Date.fromIsoString model.dateInput

                lunchMoneyInfo_ =
                    model.lunchMoneyInfo |> LunchMoneyInfo.combined
            in
            case ( token_, date_, lunchMoneyInfo_ ) of
                ( Just token, Ok date, RemoteData.Success info ) ->
                    let
                        matchingCategories =
                            LunchMoneyInfo.categories info
                                |> List.filter (\category -> category.name == model.categoryInput)

                        maybeCategoryId =
                            case matchingCategories of
                                [ match ] ->
                                    Just match.id

                                _ ->
                                    Nothing

                        maybeAssetId =
                            String.toInt model.selectedAsset

                        transaction =
                            { date = date
                            , amount =
                                model.amountInput
                                    |> String.toInt
                                    |> Maybe.withDefault 123
                                    |> LunchMoney.amountFromCents
                            , categoryId = maybeCategoryId
                            , assetId = maybeAssetId
                            }

                        ( newInsertQueue, insertCmds ) =
                            InsertQueue.insert token GotInsertQueueMsg [ transaction ] model.insertQueue
                    in
                    ( { model
                        | insertQueue = newInsertQueue
                      }
                    , insertCmds
                    )

                _ ->
                    ( model, Cmd.none )

        GotInsertQueueMsg m ->
            let
                newInsertQueue =
                    InsertQueue.update m model.insertQueue
            in
            ( { model | insertQueue = newInsertQueue }
            , storeInsertQueue newInsertQueue
            )


tokenSettingKey : String
tokenSettingKey =
    "token"


storeToken : LunchMoney.Token -> Cmd Msg
storeToken token =
    InteropDefinitions.StoreSetting
        { key = tokenSettingKey
        , value =
            Encode.encoder
                (Encode.string |> Encode.map LunchMoney.tokenToString)
                token
        }
        |> InteropPorts.fromElm


insertQueueKey : String
insertQueueKey =
    "insertQueue"


storeInsertQueue : InsertQueue -> Cmd Msg
storeInsertQueue iq =
    InteropDefinitions.StoreSetting
        { key = insertQueueKey
        , value = Encode.encoder (Codec.encoder InsertQueue.codecInsertQueue) iq
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
            case InsertQueue.processing model.insertQueue of
                InsertQueue.Idle ->
                    []

                InsertQueue.Loading _ ->
                    [ loaderView
                    ]

                InsertQueue.Error error _ ->
                    [ Html.text ("An error occurred: " ++ error) ]

        errorDisplay =
            case model.error of
                Just err ->
                    [ Html.text ("Error: " ++ err) ]

                Nothing ->
                    []

        lunchMoneyInfo =
            model.lunchMoneyInfo
                |> LunchMoneyInfo.combined

        categories =
            lunchMoneyInfo
                |> RemoteData.map LunchMoneyInfo.categories
                |> RemoteData.withDefault []

        assets =
            lunchMoneyInfo
                |> RemoteData.map LunchMoneyInfo.activeAssets
                |> RemoteData.withDefault []
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
                    [ Attr.required True ]
               ]
                |> labeled "Access token" []
             , [ dateInput model.dateInput ChangedDateInput [ Attr.required True ] ]
                |> labeled "Date" []
             , [ textInput model.amountInput
                    ChangedAmountInput
                    [ Attr.required True
                    , Attr.attribute "inputmode" "numeric"
                    ]
               ]
                |> labeled "Amount" []
             , autocompleteInput "categoryList"
                model.categoryInput
                ChangedCategoryInput
                (categories |> List.map .name)
                [ Attr.required True ]
                |> labeled "Category" [ Css.width (Css.pct 100) ]
             , selectInput
                model.selectedAsset
                ChangedAssetInput
                (assets
                    |> List.map
                        (\asset ->
                            { display = LunchMoney.assetName asset
                            , key = LunchMoney.assetId asset |> String.fromInt
                            }
                        )
                )
                |> labeled "Account" [ Css.width (Css.pct 100) ]
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


dateInput : String -> (String -> msg) -> List (Html.Attribute msg) -> Html msg
dateInput value toMsg attributes =
    Html.input
        ([ Attr.type_ "date"
         , Attr.value value
         , Event.onInput toMsg
         ]
            ++ attributes
        )
        []


autocompleteInput : String -> String -> (String -> msg) -> List String -> List (Html.Attribute msg) -> List (Html msg)
autocompleteInput listId current toMsg options attributes =
    [ textInput current toMsg ([ Attr.list listId, Event.onInput toMsg ] ++ attributes)
    , Html.datalist [ Attr.id listId ]
        (options
            |> List.map
                (\option ->
                    Html.option [ Attr.value option ] []
                )
        )
    ]


selectInput : String -> (String -> msg) -> List { display : String, key : String } -> List (Html msg)
selectInput selectedValue toMsg options =
    [ Html.select [ Event.onInput toMsg ]
        (options
            |> List.map
                (\option ->
                    Html.option
                        [ Attr.value option.key
                        , Attr.selected (option.key == selectedValue)
                        ]
                        [ Html.text option.display ]
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
