module InteropFlags exposing (Flags, autofillCacheKey, decodeFlags, flags, insertQueueKey)

import Autofill
import Date exposing (Date)
import InsertQueue exposing (InsertQueue)
import Json.Decode
import LunchMoney
import TsJson.Codec as Codec
import TsJson.Decode as Decode exposing (Decoder)
import Utils


type alias Flags =
    { today : Date
    , maybeToken : Maybe LunchMoney.Token
    , maybeInsertQueue : Maybe InsertQueue
    , maybeAutofillData : Maybe Autofill.Cache
    }


flags : Decoder Flags
flags =
    Decode.map4 Flags
        (Decode.field "today" (Codec.decoder Utils.codecDate))
        (Decode.optionalNullableField "token" tsDecodeToken)
        (Decode.optionalNullableField insertQueueKey (Codec.decoder InsertQueue.codecInsertQueue))
        (Decode.optionalNullableField autofillCacheKey (Codec.decoder Autofill.codecCache))


insertQueueKey : String
insertQueueKey =
    "insertQueue"


autofillCacheKey : String
autofillCacheKey =
    "autofillCache"


tsDecodeToken : Decoder LunchMoney.Token
tsDecodeToken =
    Decode.string
        |> Decode.andThen
            (Decode.andThenInit
                (\raw ->
                    case LunchMoney.tokenFromString raw of
                        Just token ->
                            Decode.succeed token

                        Nothing ->
                            Decode.fail ("Invalid token: '" ++ raw ++ "'")
                )
            )


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error Flags
decodeFlags flags_ =
    Json.Decode.decodeValue
        (flags |> Decode.decoder)
        flags_
