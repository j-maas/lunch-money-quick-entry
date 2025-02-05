module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import LunchMoney
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
    }


flags : Decoder Flags
flags =
    TsDecode.map2 Flags
        (TsDecode.field "today" TsDecode.string)
        (TsDecode.maybe (TsDecode.field "token" tsDecodeToken)
            |> TsDecode.map (Maybe.andThen identity)
        )


tsDecodeToken : Decoder (Maybe LunchMoney.Token)
tsDecodeToken =
    TsDecode.string
        |> TsDecode.map LunchMoney.tokenFromString


type FromElm
    = StoreSetting Setting


type ToElm
    = ReceivedSetting Setting


type alias Setting =
    { key : String
    , value : String
    }


settingCodec : Codec Setting
settingCodec =
    Codec.object Setting
        |> Codec.field "key" .key Codec.string
        |> Codec.field "value" .value Codec.string
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
