module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import InsertQueue exposing (InsertQueue)
import Json.Encode exposing (Value)
import LunchMoney
import LunchMoneyInfo
import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type alias Flags =
    { today : String
    , maybeToken : Maybe LunchMoney.Token
    , maybeInsertQueue : Maybe InsertQueue
    , maybeLunchMoneyInfo : Maybe LunchMoneyInfo.Store
    }


flags : Decoder Flags
flags =
    TsDecode.map4 Flags
        (TsDecode.field "today" TsDecode.string)
        (TsDecode.maybe (TsDecode.field "token" tsDecodeToken))
        (TsDecode.maybe (TsDecode.field "insertQueue" (Codec.decoder InsertQueue.codecInsertQueue)))
        (TsDecode.maybe (TsDecode.field "lunchMoneyInfo" (Codec.decoder LunchMoneyInfo.codecStore)))


tsDecodeToken : Decoder LunchMoney.Token
tsDecodeToken =
    TsDecode.string
        |> TsDecode.andThen
            (TsDecode.andThenInit
                (\raw ->
                    case LunchMoney.tokenFromString raw of
                        Just token ->
                            TsDecode.succeed token

                        Nothing ->
                            TsDecode.fail ("Invalid token: '" ++ raw ++ "'")
                )
            )


type FromElm
    = StoreSetting Setting


type ToElm
    = ReceivedSetting Setting


type alias Setting =
    { key : String
    , value : Value
    }


settingCodec : Codec Setting
settingCodec =
    Codec.object Setting
        |> Codec.field "key" .key Codec.string
        |> Codec.field "value" .value Codec.value
        |> Codec.buildObject


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vStoreSetting value ->
            case value of
                StoreSetting setting ->
                    vStoreSetting setting
        )
        |> TsEncode.variantTagged "storeSetting"
            (Codec.encoder settingCodec)
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "receivedSetting"
          , TsDecode.map ReceivedSetting
                (Codec.decoder settingCodec)
          )
        ]
