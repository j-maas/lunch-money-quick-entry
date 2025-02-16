module InsertQueue exposing (ClientError(..), Error(..), InsertQueue, Msg, Processing(..), ServerError(..), codecInsertQueue, empty, insert, processQueue, processing, size, update)

import Http
import LunchMoney exposing (Transaction)
import TsJson.Codec as Codec exposing (Codec)


type InsertQueue
    = InsertQueue
        { queue : List Transaction
        , processing : Processing
        }


type Processing
    = Idle
    | Loading (List Transaction)
    | Failed Error


type Error
    = NoNetwork
    | ClientError ClientError
    | ServerError ServerError
    | UnknownError String


type ClientError
    = BadClientStatus Int
    | BadUrl String
    | BadBody String


type ServerError
    = Timeout
    | BadServerStatus Int


empty : InsertQueue
empty =
    InsertQueue
        { queue = []
        , processing = Idle
        }


processing : InsertQueue -> Processing
processing (InsertQueue iq) =
    iq.processing


size : InsertQueue -> Int
size (InsertQueue iq) =
    let
        processed =
            getProcessedQueue (InsertQueue iq)
    in
    List.length iq.queue + List.length processed


codecInsertQueue : Codec InsertQueue
codecInsertQueue =
    Codec.object
        (\queue proc ->
            InsertQueue
                { queue = queue
                , processing = proc
                }
        )
        |> Codec.field "queue"
            (\(InsertQueue iq) -> iq.queue)
            (Codec.list LunchMoney.codecTransaction)
        |> Codec.field "processing"
            (\(InsertQueue iq) -> iq.processing)
            codecProcessing
        |> Codec.buildObject


codecProcessing : Codec Processing
codecProcessing =
    Codec.custom (Just "processing")
        (\idle loading value ->
            case value of
                Idle ->
                    idle

                Loading q ->
                    loading q

                Failed _ ->
                    idle
        )
        |> Codec.variant0 "idle" Idle
        |> Codec.positionalVariant1 "loading" Loading (Codec.list LunchMoney.codecTransaction)
        |> Codec.buildCustom


type Msg
    = GotResponse (Result Http.Error LunchMoney.InsertResponse)


update : Msg -> InsertQueue -> InsertQueue
update msg (InsertQueue iq) =
    case msg of
        GotResponse result ->
            case result of
                Ok _ ->
                    InsertQueue
                        { queue = iq.queue
                        , processing = Idle
                        }

                Err err ->
                    let
                        queue =
                            getProcessedQueue (InsertQueue iq) ++ iq.queue

                        error =
                            case err of
                                Http.NetworkError ->
                                    NoNetwork

                                Http.Timeout ->
                                    ServerError Timeout

                                Http.BadStatus status ->
                                    if 400 <= status && status < 500 then
                                        ClientError (BadClientStatus status)

                                    else if 500 <= status && status < 600 then
                                        ServerError (BadServerStatus status)

                                    else
                                        UnknownError ("Bad status code: " ++ String.fromInt status)

                                Http.BadUrl badUrl ->
                                    ClientError (BadUrl badUrl)

                                Http.BadBody badBody ->
                                    ClientError (BadBody badBody)
                    in
                    InsertQueue
                        { queue = queue
                        , processing = Failed error
                        }


getProcessedQueue : InsertQueue -> List Transaction
getProcessedQueue (InsertQueue iq) =
    case iq.processing of
        Idle ->
            []

        Loading t ->
            t

        Failed _ ->
            []


insert : LunchMoney.Token -> (Msg -> msg) -> List Transaction -> InsertQueue -> ( InsertQueue, Cmd msg )
insert token toMsg transactions insertQueue =
    add transactions insertQueue
        |> processQueue token toMsg


add : List Transaction -> InsertQueue -> InsertQueue
add transactions (InsertQueue iq) =
    InsertQueue { iq | queue = iq.queue ++ transactions }


processQueue : LunchMoney.Token -> (Msg -> msg) -> InsertQueue -> ( InsertQueue, Cmd msg )
processQueue token toMsg (InsertQueue iq) =
    case iq.processing of
        Loading _ ->
            ( InsertQueue iq, Cmd.none )

        _ ->
            ( InsertQueue
                { queue = []
                , processing = Loading iq.queue
                }
            , Cmd.batch
                [ LunchMoney.insertTransactions token iq.queue (GotResponse >> toMsg)
                ]
            )
