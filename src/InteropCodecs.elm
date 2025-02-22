module InteropCodecs exposing (FromElm(..), LogLevel(..), ToElm(..), fromElm, toElm)

import Json.Encode exposing (Value)
import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as Decode exposing (Decoder)
import TsJson.Encode as Encode exposing (Encoder)


type FromElm
    = StoreSetting Setting
    | Log { level : LogLevel, message : String }


type LogLevel
    = Info
    | Warning
    | Error
    | Debugging


encodeLogLevel : Encode.Encoder LogLevel
encodeLogLevel =
    Encode.union
        (\vInfo vWarning vError vDebug value ->
            case value of
                Info ->
                    vInfo

                Warning ->
                    vWarning

                Error ->
                    vError

                Debugging ->
                    vDebug
        )
        |> Encode.variantLiteral (Json.Encode.string "Info")
        |> Encode.variantLiteral (Json.Encode.string "Warning")
        |> Encode.variantLiteral (Json.Encode.string "Error")
        |> Encode.variantLiteral (Json.Encode.string "Debug")
        |> Encode.buildUnion


fromElm : Encoder FromElm
fromElm =
    Encode.union
        (\vStoreSetting vLog value ->
            case value of
                StoreSetting setting ->
                    vStoreSetting setting

                Log payload ->
                    vLog { level = payload.level, message = payload.message }
        )
        |> Encode.variantTagged "storeSetting"
            (Codec.encoder settingCodec)
        |> Encode.variantTagged "log"
            (Encode.object
                [ Encode.required "level" .level encodeLogLevel
                , Encode.required "message" .message Encode.string
                ]
            )
        |> Encode.buildUnion


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


toElm : Decoder ToElm
toElm =
    Decode.discriminatedUnion "tag"
        [ ( "receivedSetting"
          , Decode.map ReceivedSetting
                (Codec.decoder settingCodec)
          )
        ]
