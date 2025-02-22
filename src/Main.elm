module Main exposing (main)

import Autofill
import Browser
import Char exposing (isDigit)
import Css
import Date exposing (Date)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import InsertQueue exposing (InsertQueue)
import InteropCodecs
import InteropFlags
import InteropPorts
import Json.Decode as Decode
import LunchMoney
import Time
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
    , today : Date
    , dateInput : String
    , autofill : Autofill.Cache
    , payeeInput : String
    , selectedCategory : String
    , selectedAsset : String
    , amountInput : String
    , inflowInput : Bool
    , insertQueue : InsertQueue
    , error : Maybe String
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsRaw =
    case InteropFlags.decodeFlags flagsRaw of
        Err flagsError ->
            ( Err <| Decode.errorToString flagsError
            , Cmd.none
            )

        Ok flags ->
            let
                autofill =
                    flags.maybeAutofillData |> Maybe.withDefault Autofill.empty

                ( maybeError, maybeAutofillCmd ) =
                    case flags.maybeToken of
                        Just token ->
                            ( Nothing, Autofill.refresh token flags.today GotAutofillMsg autofill )

                        _ ->
                            ( Just "No token given at startup or invalid date, not getting updates.", Cmd.none )
            in
            ( Ok
                { token = flags.maybeToken
                , today = flags.today
                , dateInput = flags.today |> Date.toIsoString
                , autofill = autofill
                , amountInput = "0,00"
                , inflowInput = False
                , payeeInput = ""
                , selectedCategory = ""
                , selectedAsset = ""
                , insertQueue =
                    flags.maybeInsertQueue
                        |> Maybe.withDefault InsertQueue.empty
                , error = maybeError
                }
            , maybeAutofillCmd
            )


type Msg
    = GotAutofillMsg Autofill.Msg
    | ChangedToken String
    | ChangedDateInput String
    | ChangedAmountInput String
    | ChangedInflowInput Bool
    | ChangedPayeeInput String
    | ChangedCategoryInput String
    | ChangedAssetInput String
    | TappedInsertTransaction
    | TappedProcessQueue
    | TappedSyncAutofill
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
        GotAutofillMsg m ->
            let
                ( newAutofillData, autofillCmds ) =
                    Autofill.update m GotAutofillMsg model.autofill

                selectedAsset =
                    case newAutofillData |> Autofill.combined of
                        ( _, Just data ) ->
                            case Autofill.activeAssets data.info of
                                first :: _ ->
                                    LunchMoney.assetId first |> String.fromInt

                                _ ->
                                    ""

                        _ ->
                            ""
            in
            ( { model
                | autofill = newAutofillData
                , selectedAsset = selectedAsset
              }
            , Cmd.batch
                [ autofillCmds
                , storeAutofill newAutofillData
                ]
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
            let
                cleanedNewAmount =
                    cleanAmountInput newAmount
                        |> String.fromInt

                beforeDecimal =
                    String.dropRight 2 cleanedNewAmount
                        |> String.padLeft 1 '0'

                afterDecimal =
                    String.right 2 cleanedNewAmount
                        |> String.padLeft 2 '0'

                formattedNewAmount =
                    beforeDecimal ++ "," ++ afterDecimal
            in
            ( { model | amountInput = formattedNewAmount }, Cmd.none )

        ChangedInflowInput newInflow ->
            ( { model | inflowInput = newInflow }, Cmd.none )

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

                        inflowFactor =
                            if model.inflowInput then
                                -1

                            else
                                1

                        cents =
                            cleanAmountInput model.amountInput * inflowFactor

                        transaction =
                            { date = date
                            , amount =
                                LunchMoney.amountFromCents cents
                            , payee =
                                if String.isEmpty model.payeeInput then
                                    Nothing

                                else
                                    Just model.payeeInput
                            , categoryId = maybeCategoryId
                            , assetId = maybeAssetId
                            , status = Just LunchMoney.Cleared
                            }

                        ( newInsertQueue, insertCmds ) =
                            InsertQueue.insert token GotInsertQueueMsg [ transaction ] model.insertQueue
                    in
                    ( { model
                        | insertQueue = newInsertQueue
                        , amountInput = ""
                        , payeeInput = ""
                        , selectedCategory = ""
                        , selectedAsset = ""
                      }
                    , insertCmds
                    )

                _ ->
                    ( model, Cmd.none )

        TappedProcessQueue ->
            let
                token_ =
                    model.token
            in
            case token_ of
                Just token ->
                    let
                        ( newInsertQueue, iqCmds ) =
                            InsertQueue.processQueue token GotInsertQueueMsg model.insertQueue
                    in
                    ( { model | insertQueue = newInsertQueue }
                    , iqCmds
                    )

                _ ->
                    ( model, Cmd.none )

        TappedSyncAutofill ->
            case model.token of
                Just token ->
                    ( model
                    , Autofill.fetch token model.today GotAutofillMsg
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


cleanAmountInput : String -> Int
cleanAmountInput amountInput =
    String.filter isDigit amountInput
        |> String.toInt
        |> Maybe.withDefault 0


tokenSettingKey : String
tokenSettingKey =
    "token"


storeToken : LunchMoney.Token -> Cmd Msg
storeToken token =
    InteropCodecs.StoreSetting
        { key = tokenSettingKey
        , value =
            Encode.encoder
                (Encode.string |> Encode.map LunchMoney.tokenToString)
                token
        }
        |> InteropPorts.fromElm


storeInsertQueue : InsertQueue -> Cmd Msg
storeInsertQueue iq =
    InteropCodecs.StoreSetting
        { key = InteropFlags.insertQueueKey
        , value = Encode.encoder (Codec.encoder InsertQueue.codecInsertQueue) iq
        }
        |> InteropPorts.fromElm


storeAutofill : Autofill.Cache -> Cmd Msg
storeAutofill store =
    InteropCodecs.StoreSetting
        { key = InteropFlags.autofillCacheKey
        , value = Encode.encoder (Codec.encoder Autofill.codecCache) store
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
            let
                displayUnsent error =
                    Html.p []
                        [ Html.text
                            (String.fromInt (InsertQueue.size model.insertQueue)
                                ++ " transactions to be inserted, stored offline."
                            )
                        ]
                        :: error
                        ++ [ Html.button
                                [ Events.onClick TappedProcessQueue
                                , Attr.type_ "button"
                                ]
                                [ Html.text "Try again" ]
                           ]
            in
            case InsertQueue.processing model.insertQueue of
                InsertQueue.Idle ->
                    if InsertQueue.isEmpty model.insertQueue then
                        []

                    else
                        displayUnsent []

                InsertQueue.Loading _ ->
                    [ loaderView
                    ]

                InsertQueue.Failed error ->
                    displayUnsent [ Html.p [] [ Html.text (displayInsertQueueError error) ] ]

        ( autofillError, autofillData ) =
            model.autofill
                |> Autofill.combined

        lunchMoneyErrorDisplay =
            case autofillError of
                Just error ->
                    [ Html.text ("Error getting info: " ++ error) ]

                Nothing ->
                    []

        errorDisplay =
            case model.error of
                Just err ->
                    Html.text ("Error: " ++ err) :: lunchMoneyErrorDisplay

                Nothing ->
                    [] ++ lunchMoneyErrorDisplay

        payees =
            autofillData
                |> Maybe.map .info
                |> Maybe.map Autofill.payees
                |> Maybe.withDefault []

        categories =
            autofillData
                |> Maybe.map .info
                |> Maybe.map Autofill.categories
                |> Maybe.withDefault []

        assets =
            autofillData
                |> Maybe.map .info
                |> Maybe.map Autofill.activeAssets
                |> Maybe.withDefault []
    in
    Html.main_
        [ Attr.css
            [ columnStyle 3
            , Css.alignItems Css.center
            , Css.height (Css.pct 100)
            , Css.padding (Css.rem 1)
            ]
        ]
        [ Html.div [ Attr.css [ Css.width (Css.pct 100) ] ]
            (Html.form
                [ Attr.css
                    [ columnStyle 1
                    , Css.alignItems Css.stretch
                    , Css.width (Css.pct 100)
                    ]
                , Events.onSubmit TappedInsertTransaction
                ]
                [ labeled "Date"
                    []
                    [ dateInput model.dateInput ChangedDateInput [ Attr.required True ] ]
                , labeled "Amount"
                    []
                    [ textInput model.amountInput
                        ChangedAmountInput
                        [ Attr.attribute "inputmode" "numeric"
                        ]
                    ]
                , Html.div [ Attr.css [ Css.display Css.flex_, Css.flexDirection Css.row, Css.gap (Css.rem 1) ] ]
                    [ Html.label []
                        [ Html.input
                            [ Attr.type_ "radio"
                            , Attr.name "inflow"
                            , Attr.value "outflow"
                            , Attr.checked (not model.inflowInput)
                            , Events.onInput (\_ -> ChangedInflowInput False)
                            ]
                            []
                        , Html.text "Outflow"
                        ]
                    , Html.label []
                        [ Html.input
                            [ Attr.type_ "radio"
                            , Attr.name "inflow"
                            , Attr.value "inflow"
                            , Attr.checked model.inflowInput
                            , Events.onInput (\_ -> ChangedInflowInput True)
                            ]
                            []
                        , Html.text "Inflow"
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
                :: insertionIndicator
                ++ errorDisplay
            )
        , Html.div [ Attr.css [ Css.width (Css.pct 100) ] ]
            [ autofillView model.today model.autofill
            , settingsView model.token []
            ]
        ]


columnStyle : Float -> Css.Style
columnStyle gap =
    Css.batch
        [ Css.display Css.flex_
        , Css.flexDirection Css.column
        , Css.gap (Css.rem gap)
        ]


textInput : String -> (String -> msg) -> List (Html.Attribute msg) -> Html msg
textInput value toMsg attributes =
    Html.input
        ([ Attr.type_ "text"
         , Attr.value value
         , Events.onInput toMsg
         ]
            ++ attributes
        )
        []


dateInput : String -> (String -> msg) -> List (Html.Attribute msg) -> Html msg
dateInput value toMsg attributes =
    Html.input
        ([ Attr.type_ "date"
         , Attr.value value
         , Events.onInput toMsg
         ]
            ++ attributes
        )
        []


autocompleteInput : String -> String -> (String -> msg) -> List (Html.Attribute msg) -> List String -> List (Html msg)
autocompleteInput listId current toMsg attributes options =
    [ textInput current toMsg ([ Attr.list listId, Events.onInput toMsg ] ++ attributes)
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
    Html.select (Events.onInput toMsg :: attributes)
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


displayInsertQueueError : InsertQueue.Error -> String
displayInsertQueueError error =
    case error of
        InsertQueue.NoNetwork ->
            "Please check your internet connection."

        InsertQueue.ClientError err ->
            "I did something wrong: "
                ++ (case err of
                        InsertQueue.BadClientStatus status ->
                            "Bad status code: " ++ String.fromInt status

                        InsertQueue.BadUrl url ->
                            "Bad url: " ++ url

                        InsertQueue.BadBody body ->
                            "Bad body: " ++ body
                   )

        InsertQueue.ServerError err ->
            "The server is not available: "
                ++ (case err of
                        InsertQueue.BadServerStatus status ->
                            "Bad status code: " ++ String.fromInt status

                        InsertQueue.Timeout ->
                            "Timeout"
                   )

        InsertQueue.UnknownError err ->
            "Something went wrong: " ++ err


autofillView : Date -> Autofill.Cache -> Html Msg
autofillView today cache =
    let
        ( _, maybeData ) =
            Autofill.combined cache

        maybeLastUpdated =
            maybeData
                |> Maybe.map
                    (\data ->
                        data.lastUpdated
                            |> Date.fromPosix Time.utc
                    )

        diffDisplay lastUpdated =
            let
                daysDiff =
                    Date.diff Date.Days lastUpdated today
            in
            if daysDiff == 0 then
                "Synced today."

            else if daysDiff == 1 then
                "Synced yesterday."

            else if daysDiff < 7 then
                "Synced " ++ String.fromInt daysDiff ++ " days ago."

            else
                "Last sync was more than a week ago."

        displayLastUpdated =
            case maybeLastUpdated of
                Just lastUpdated ->
                    diffDisplay lastUpdated

                Nothing ->
                    "No autofill data available."
    in
    Html.p
        [ Attr.css
            [ Css.display Css.flex_
            , Css.flexWrap Css.wrap
            , Css.gap (Css.rem 0.5)
            ]
        ]
        [ Html.text displayLastUpdated
        , Html.button
            [ Events.onClick TappedSyncAutofill
            ]
            [ Html.text "Sync now" ]
        ]


settingsView : Maybe LunchMoney.Token -> List Css.Style -> Html Msg
settingsView maybeToken styles =
    Html.details [ Attr.css styles ]
        [ Html.summary
            [ Attr.css
                [ Css.marginBottom (Css.rem 1)
                ]
            ]
            [ Html.text "Settings" ]
        , labeled "Access token"
            []
            [ textInput
                (case maybeToken of
                    Just token ->
                        LunchMoney.tokenToString token

                    Nothing ->
                        ""
                )
                ChangedToken
                [ Attr.required True ]
            ]
        ]
