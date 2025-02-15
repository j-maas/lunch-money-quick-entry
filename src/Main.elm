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
    , payeeInput : String
    , selectedCategory : String
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
                    case ( flags.maybeToken, Date.fromIsoString flags.today ) of
                        ( Just token, Ok today ) ->
                            ( Nothing, LunchMoneyInfo.fetch token today GotLunchMoneyInfoMsg )

                        _ ->
                            ( Just "No token given at startup or invalid date, not getting updates.", Cmd.none )
            in
            ( Ok
                { token = flags.maybeToken
                , dateInput = flags.today
                , lunchMoneyInfo = LunchMoneyInfo.empty
                , amountInput = ""
                , payeeInput = ""
                , selectedCategory = ""
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
    | ChangedPayeeInput String
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

        ChangedPayeeInput newPayee ->
            ( { model | payeeInput = newPayee }, Cmd.none )

        ChangedCategoryInput newCategory ->
            ( { model | selectedCategory = newCategory }, Cmd.none )

        ChangedAssetInput newAssetId ->
            ( { model | selectedAsset = newAssetId }, Cmd.none )

        TappedInsertTransaction ->
            let
                token_ =
                    model.token

                date_ =
                    Date.fromIsoString model.dateInput
            in
            case ( token_, date_ ) of
                ( Just token, Ok date ) ->
                    let
                        maybeCategoryId =
                            String.toInt model.selectedCategory

                        maybeAssetId =
                            String.toInt model.selectedAsset

                        transaction =
                            { date = date
                            , amount =
                                model.amountInput
                                    |> String.toInt
                                    |> Maybe.withDefault 123
                                    |> LunchMoney.amountFromCents
                            , payee =
                                if String.isEmpty model.payeeInput then
                                    Nothing

                                else
                                    Just model.payeeInput
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

        payees =
            lunchMoneyInfo
                |> RemoteData.map LunchMoneyInfo.payees
                |> RemoteData.withDefault []

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
            ([ labeled "Access token"
                []
                [ textInput
                    (case model.token of
                        Just token ->
                            LunchMoney.tokenToString token

                        Nothing ->
                            ""
                    )
                    ChangedToken
                    [ Attr.required True ]
                ]
             , labeled "Date"
                []
                [ dateInput model.dateInput ChangedDateInput [ Attr.required True ] ]
             , labeled "Amount"
                []
                [ textInput model.amountInput
                    ChangedAmountInput
                    [ Attr.required True
                    , Attr.attribute "inputmode" "numeric"
                    ]
                ]
             , labeled "Payee"
                []
                (autocompleteInput
                    "payeeList"
                    model.payeeInput
                    ChangedPayeeInput
                    []
                    payees
                )
             , labeled "Category"
                []
                [ selectInput
                    ChangedCategoryInput
                    []
                    (groupedOptions
                        model.selectedCategory
                        (( "", [ { display = "Uncategorized", key = "" } ] )
                            :: (categories
                                    |> List.map
                                        (Tuple.mapSecond
                                            (List.map
                                                (\category ->
                                                    { display = category.name
                                                    , key = category.id |> String.fromInt
                                                    }
                                                )
                                            )
                                        )
                               )
                        )
                    )
                ]
             , labeled "Account"
                []
                [ selectInput
                    ChangedAssetInput
                    []
                    (flatOptions model.selectedAsset
                        (assets
                            |> List.map
                                (\asset ->
                                    { display = LunchMoney.assetName asset
                                    , key = LunchMoney.assetId asset |> String.fromInt
                                    }
                                )
                        )
                    )
                ]
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


autocompleteInput : String -> String -> (String -> msg) -> List (Html.Attribute msg) -> List String -> List (Html msg)
autocompleteInput listId current toMsg attributes options =
    [ textInput current toMsg ([ Attr.list listId, Event.onInput toMsg ] ++ attributes)
    , Html.datalist [ Attr.id listId ]
        (options
            |> List.map
                (\option ->
                    Html.option [ Attr.value option ] []
                )
        )
    ]


selectInput : (String -> msg) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
selectInput toMsg attributes options =
    Html.select (Event.onInput toMsg :: attributes)
        options


flatOptions : String -> List { display : String, key : String } -> List (Html msg)
flatOptions selectedValue options =
    options
        |> List.map
            (\option ->
                Html.option
                    [ Attr.value option.key
                    , Attr.selected (option.key == selectedValue)
                    ]
                    [ Html.text option.display ]
            )


groupedOptions : String -> List ( String, List { display : String, key : String } ) -> List (Html msg)
groupedOptions selectedValue options =
    options
        |> List.concatMap
            (\( groupName, children ) ->
                if String.isEmpty groupName then
                    flatOptions selectedValue children

                else
                    [ Html.optgroup [ Attr.attribute "label" groupName ]
                        (flatOptions selectedValue children)
                    ]
            )


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
