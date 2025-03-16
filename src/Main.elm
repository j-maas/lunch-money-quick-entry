module Main exposing (main)

import Autofill
import Browser
import Char exposing (isDigit)
import Css
import Date exposing (Date)
import Dict exposing (Dict)
import Forms.Field as Field exposing (Fields)
import Forms.Form as Form exposing (Form)
import Forms.Update as Update
import Forms.Validation as Val
import Forms.Validation.Result as FormResult
import Forms.Value
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
    , notesInput : String
    , inflowInput : Bool
    , inputForm : InputForm
    , settingsForm : SettingsForm
    , insertQueue : InsertQueue
    , error : Maybe String
    }


type alias InputForm =
    Form String FormError InputFormResult


inputFormFields : { date : Date } -> Fields String
inputFormFields default =
    Field.fields
        [ ( "date", Field.inputWithDefault (Date.toIsoString default.date) )
        , ( "amount", Field.inputWithDefault "0,00" )
        , ( "amountDirection", Field.inputWithDefault "outflow" )
        , ( "payee", Field.input )
        , ( "categoryId", Field.input )
        , ( "assetId", Field.input )
        , ( "notes", Field.input )
        ]


type alias InputFormResult =
    LunchMoney.Transaction


type FormError
    = IsEmpty
    | NotInt
    | InvalidDate String
    | InvalidToken


inputFormValidate : Val.Validate String FormError InputFormResult
inputFormValidate fields =
    Val.valid LunchMoney.Transaction
        |> Val.required fields
            "date"
            (Val.stringField <|
                Val.notEmpty IsEmpty <|
                    \raw ->
                        case Date.fromIsoString raw of
                            Ok date ->
                                Val.success date

                            Err err ->
                                Val.failure (InvalidDate err)
            )
        |> Val.twoFields fields
            "amount"
            "amountDirection"
            (\amountValue amountDirectionValue ->
                let
                    amountRaw =
                        Forms.Value.getString amountValue |> Maybe.withDefault ""

                    amountDirectionRaw =
                        Forms.Value.getString amountDirectionValue
                            |> Maybe.withDefault ""

                    amountDirection =
                        if amountDirectionRaw == "outflow" then
                            1

                        else
                            -1

                    cents =
                        cleanAmountInput amountRaw * amountDirection
                in
                LunchMoney.amountFromCents cents
                    |> Val.success
            )
        |> Val.optionalWithMaybe fields "payee" Val.success
        |> Val.optionalWithMaybe fields "categoryId" (Val.int NotInt <| Val.success)
        |> Val.optionalWithMaybe fields "notes" Val.success
        |> Val.optionalWithMaybe fields "assetId" (Val.int NotInt <| Val.success)
        |> Val.hardcoded (Just LunchMoney.Cleared)


type alias SettingsForm =
    Form String FormError SettingsFormResult


settingsFormFields : SettingsFormResult -> Fields String
settingsFormFields default =
    Field.fields
        [ ( "token"
          , Field.inputWithDefault
                (default.token
                    |> Maybe.map LunchMoney.tokenToString
                    |> Maybe.withDefault ""
                )
          )
        ]


type alias SettingsFormResult =
    { token : Maybe LunchMoney.Token }


settingsFormValidate : Val.Validate String FormError SettingsFormResult
settingsFormValidate fields =
    Val.valid SettingsFormResult
        |> Val.optionalWithMaybe fields
            "token"
            (\raw ->
                case LunchMoney.tokenFromString raw of
                    Just token ->
                        Val.success token

                    Nothing ->
                        Val.failure InvalidToken
            )


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

                inputFields =
                    inputFormFields { date = flags.today }

                settingsFields =
                    settingsFormFields { token = flags.maybeToken }
            in
            ( Ok
                { token = flags.maybeToken
                , today = flags.today
                , dateInput = flags.today |> Date.toIsoString
                , autofill = autofill
                , amountInput = "0,00"
                , inflowInput = False
                , payeeInput = ""
                , notesInput = ""
                , selectedCategory = ""
                , selectedAsset = ""
                , inputForm = Form.form inputFields inputFormValidate
                , settingsForm = Form.form settingsFields settingsFormValidate
                , insertQueue =
                    flags.maybeInsertQueue
                        |> Maybe.withDefault InsertQueue.empty
                , error = maybeError
                }
            , maybeAutofillCmd
            )


type Msg
    = GotAutofillMsg Autofill.Msg
    | TappedInsertTransaction
    | TappedProcessQueue
    | TappedSyncAutofill
    | GotInsertQueueMsg InsertQueue.Msg
    | InputFormChanged (Update.Msg String)
    | SettingsFormChanged (Update.Msg String)


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

        TappedInsertTransaction ->
            let
                token_ =
                    model.token

                formResult =
                    Form.validate model.inputForm
            in
            case ( token_, formResult ) of
                ( Just token, FormResult.Valid transaction ) ->
                    let
                        ( newInsertQueue, insertCmds ) =
                            InsertQueue.insert token GotInsertQueueMsg [ transaction ] model.insertQueue
                    in
                    ( { model
                        | insertQueue = newInsertQueue
                        , inputForm = Form.form (inputFormFields { date = model.today }) inputFormValidate
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

        InputFormChanged formMsg ->
            let
                newInputForm =
                    Update.updateForm formMsg model.inputForm

                formattedNewAmount =
                    Form.getStringField "amount" newInputForm
                        |> Maybe.map
                            (\newAmount ->
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
                                in
                                beforeDecimal ++ "," ++ afterDecimal
                            )
                        |> Maybe.withDefault ""

                newInputForm2 =
                    Form.setStringField "amount" formattedNewAmount newInputForm
            in
            ( { model | inputForm = newInputForm2 }, Cmd.none )

        SettingsFormChanged formMsg ->
            let
                newSettingsForm =
                    Update.updateForm formMsg model.settingsForm

                settingsResult =
                    Form.validate newSettingsForm

                maybeToken =
                    case settingsResult of
                        FormResult.Valid settings ->
                            settings.token

                        _ ->
                            Nothing

                maybeStoreToken =
                    case maybeToken of
                        Just token ->
                            storeToken token

                        Nothing ->
                            Cmd.none
            in
            ( { model | settingsForm = newSettingsForm }
            , maybeStoreToken
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

        inputFormErrors =
            errorsFromValidation (Form.validate model.inputForm)
                |> List.map Html.text

        settingsFormErrors =
            errorsFromValidation (Form.validate model.settingsForm)
                |> List.map Html.text

        lunchMoneyErrorDisplay =
            case autofillError of
                Just error ->
                    [ Html.text ("Error getting info: " ++ error) ]

                Nothing ->
                    []

        errorDisplay =
            case model.error of
                Just err ->
                    Html.text ("Error: " ++ err) :: lunchMoneyErrorDisplay ++ inputFormErrors ++ settingsFormErrors

                Nothing ->
                    [] ++ lunchMoneyErrorDisplay ++ inputFormErrors ++ settingsFormErrors

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
                    [ dateInput ( "date", model.inputForm ) InputFormChanged [ Attr.required True ] ]
                , labeled "Amount"
                    []
                    [ textInput ( "amount", model.inputForm )
                        InputFormChanged
                        [ Attr.attribute "inputmode" "numeric"
                        ]
                    ]
                , Html.div [ Attr.css [ Css.display Css.flex_, Css.flexDirection Css.row, Css.gap (Css.rem 1) ] ]
                    [ Html.label []
                        [ radioInput ( "amountDirection", model.inputForm ) "outflow" InputFormChanged []
                        , Html.text "Outflow"
                        ]
                    , Html.label []
                        [ radioInput ( "amountDirection", model.inputForm ) "inflow" InputFormChanged []
                        , Html.text "Inflow"
                        ]
                    ]
                , labeled "Payee"
                    []
                    (autocompleteInput
                        "payeeList"
                        ( "payee", model.inputForm )
                        InputFormChanged
                        []
                        payees
                    )
                , labeled "Category"
                    []
                    [ selectInput
                        ( "categoryId", model.inputForm )
                        InputFormChanged
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
                        ( "assetId", model.inputForm )
                        InputFormChanged
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
                , labeled "Notes"
                    []
                    [ textInput ( "notes", model.inputForm )
                        InputFormChanged
                        []
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
            , settingsView model.settingsForm []
            ]
        ]


errorsFromValidation : FormResult.FormResult String FormError result -> List String
errorsFromValidation result =
    case result of
        FormResult.Valid _ ->
            []

        FormResult.Invalid errors ->
            displayAllFormErrors
                formErrorToString
                errors

        FormResult.Error errors ->
            "The form was misconfigured!"
                :: displayAllFormErrors
                    (\error ->
                        case error of
                            FormResult.MissingField ->
                                "Is missing."

                            FormResult.WrongType ->
                                "Is of the wrong type."
                    )
                    errors


displayAllFormErrors : (error -> String) -> Dict String error -> List String
displayAllFormErrors toString errors =
    Dict.toList errors
        |> List.map
            (\( key, error ) ->
                key ++ ": " ++ toString error
            )


formErrorToString : FormError -> String
formErrorToString error =
    case error of
        IsEmpty ->
            "Should not be empty."

        NotInt ->
            "Should be a whole number."

        InvalidDate err ->
            "Should be a valid date. " ++ err

        InvalidToken ->
            "Should be a valid access token."


columnStyle : Float -> Css.Style
columnStyle gap =
    Css.batch
        [ Css.display Css.flex_
        , Css.flexDirection Css.column
        , Css.gap (Css.rem gap)
        ]


type alias FormInput error result =
    ( String, Form String error result )


textInput : FormInput error result -> (Update.Msg String -> Msg) -> List (Html.Attribute Msg) -> Html Msg
textInput formInput toMsg attributes =
    inputField "text" formInput toMsg attributes


radioInput : FormInput error result -> String -> (Update.Msg String -> Msg) -> List (Html.Attribute Msg) -> Html Msg
radioInput ( fieldName, form ) value toMsg attributes =
    let
        currentValue =
            Form.getStringField fieldName form
                |> Maybe.withDefault ""
    in
    Html.input
        ([ Attr.type_ "radio"
         , Attr.value value
         , Attr.name fieldName
         , Attr.checked (currentValue == value)
         , Events.onInput (Update.stringFieldMsg toMsg fieldName)
         ]
            ++ attributes
        )
        []


dateInput : FormInput error result -> (Update.Msg String -> Msg) -> List (Html.Attribute Msg) -> Html Msg
dateInput formInput toMsg attributes =
    inputField "date" formInput toMsg attributes


autocompleteInput : String -> FormInput error result -> (Update.Msg String -> Msg) -> List (Html.Attribute Msg) -> List String -> List (Html Msg)
autocompleteInput listId formInput toMsg attributes options =
    [ textInput formInput toMsg (Attr.list listId :: attributes)
    , Html.datalist [ Attr.id listId ]
        (options
            |> List.map
                (\option ->
                    Html.option [ Attr.value option ] []
                )
        )
    ]


selectInput : FormInput error result -> (Update.Msg String -> msg) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
selectInput ( fieldName, _ ) toMsg attributes options =
    Html.select (Events.onInput (Update.stringFieldMsg toMsg fieldName) :: attributes)
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


inputField : String -> FormInput error result -> (Update.Msg String -> Msg) -> List (Html.Attribute Msg) -> Html Msg
inputField type_ ( fieldName, form ) toMsg attributes =
    let
        currentValue =
            Form.getStringField fieldName form
                |> Maybe.withDefault ""
    in
    Html.input
        ([ Attr.type_ type_
         , Attr.value currentValue
         , Events.onInput (Update.stringFieldMsg toMsg fieldName)
         ]
            ++ attributes
        )
        []


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


settingsView : SettingsForm -> List Css.Style -> Html Msg
settingsView settingsForm styles =
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
                ( "token", settingsForm )
                SettingsFormChanged
                [ Attr.required True ]
            ]
        ]
