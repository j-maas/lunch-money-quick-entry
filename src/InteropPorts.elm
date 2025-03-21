port module InteropPorts exposing (fromElm, toElm)

{-| This file was generated by elm-ts-interop init. You can manually edit this file,
just avoid exposing the low-level ports to ensure that only the FromElm and ToElm types
can be sent/received by your ports.

@docs fromElm, toElm, decodeFlags

-}

import InteropCodecs
import Json.Decode
import Json.Encode
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


{-| -}
fromElm : InteropCodecs.FromElm -> Cmd msg
fromElm value =
    value
        |> (InteropCodecs.fromElm |> TsEncode.encoder)
        |> interopFromElm


{-| -}
toElm : Sub (Result Json.Decode.Error InteropCodecs.ToElm)
toElm =
    (InteropCodecs.toElm |> TsDecode.decoder)
        |> Json.Decode.decodeValue
        |> interopToElm



-- internals - do not expose


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
