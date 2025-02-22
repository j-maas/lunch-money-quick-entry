module InteropDefinitions exposing (interop)

import InteropCodecs exposing (FromElm, ToElm)
import InteropFlags exposing (Flags)
import TsJson.Decode exposing (Decoder)
import TsJson.Encode exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = InteropCodecs.toElm
    , fromElm = InteropCodecs.fromElm
    , flags = InteropFlags.flags
    }
