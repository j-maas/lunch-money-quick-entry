module Utils.Cached exposing (Cached(..), combine3, toMaybe, updateWithResult, withDefault)


type Cached e a
    = Missing
    | Loading
    | LoadingError e
    | Available a
    | RefreshError e a


withDefault : a -> Cached e a -> a
withDefault default cached =
    case cached of
        Missing ->
            default

        Loading ->
            default

        LoadingError _ ->
            default

        Available value ->
            value

        RefreshError _ value ->
            value


updateWithResult : Cached e a -> Result e a -> Cached e a
updateWithResult cached result =
    case result of
        Ok value ->
            Available value

        Err error ->
            case cached of
                Missing ->
                    LoadingError error

                Loading ->
                    LoadingError error

                LoadingError _ ->
                    LoadingError error

                Available oldValue ->
                    RefreshError error oldValue

                RefreshError _ oldValue ->
                    RefreshError error oldValue


toMaybe : Cached e a -> Maybe a
toMaybe cached =
    case cached of
        Missing ->
            Nothing

        Loading ->
            Nothing

        LoadingError _ ->
            Nothing

        Available value ->
            Just value

        RefreshError _ value ->
            Just value


getError : Cached e a -> Maybe e
getError cached =
    case cached of
        Missing ->
            Nothing

        Loading ->
            Nothing

        LoadingError error ->
            Just error

        Available _ ->
            Nothing

        RefreshError error _ ->
            Just error


combine3 : (a -> b -> c -> d) -> Cached e a -> Cached e b -> Cached e c -> ( Maybe e, Maybe d )
combine3 mapping first second third =
    let
        maybeValue =
            Maybe.map3 mapping
                (toMaybe first)
                (toMaybe second)
                (toMaybe third)

        maybeError =
            getError first
                |> Maybe.andThen (\_ -> getError second)
                |> Maybe.andThen (\_ -> getError third)
    in
    ( maybeError, maybeValue )
