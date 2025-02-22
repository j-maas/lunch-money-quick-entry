module Log exposing (debug, error, info, warning)

import InteropCodecs
import InteropPorts


info : String -> Cmd msg
info message =
    logHelper InteropCodecs.Info message


warning : String -> Cmd msg
warning message =
    logHelper InteropCodecs.Warning message


error : String -> Cmd msg
error message =
    logHelper InteropCodecs.Error message


debug : String -> Cmd msg
debug message =
    logHelper InteropCodecs.Debugging message


logHelper : InteropCodecs.LogLevel -> String -> Cmd msg
logHelper level message =
    InteropCodecs.Log
        { level = level
        , message = message
        }
        |> InteropPorts.fromElm
