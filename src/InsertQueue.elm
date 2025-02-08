module InsertQueue exposing (InsertQueue, Msg, Processing(..), codecInsertQueue, empty, insert, processing, update)

import Http
import LunchMoney exposing (Transaction)
import TsJson.Codec as Codec exposing (Codec)
import Utils exposing (stringFromHttpError)


type InsertQueue
    = InsertQueue
        { queue : List Transaction
        , processing : Processing
        }


type Processing
    = Idle
    | Loading (List Transaction)
    | Error String (List Transaction)


empty : InsertQueue
empty =
    InsertQueue
        { queue = []
        , processing = Idle
        }


processing : InsertQueue -> Processing
processing (InsertQueue iq) =
    iq.processing


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
        (\idle loading error value ->
            case value of
                Idle ->
                    idle

                Loading q ->
                    loading q

                Error e q ->
                    error e q
        )
        |> Codec.variant0 "idle" Idle
        |> Codec.positionalVariant1 "loading" Loading (Codec.list LunchMoney.codecTransaction)
        |> Codec.positionalVariant2 "error" Error Codec.string (Codec.list LunchMoney.codecTransaction)
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
                    InsertQueue
                        { queue = iq.queue
                        , processing = Error (stringFromHttpError err) (getProcessedQueue (InsertQueue iq))
                        }


getProcessedQueue : InsertQueue -> List Transaction
getProcessedQueue (InsertQueue iq) =
    case iq.processing of
        Idle ->
            []

        Loading t ->
            t

        Error _ t ->
            t


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
        Idle ->
            ( InsertQueue
                { queue = []
                , processing = Loading iq.queue
                }
            , Cmd.batch
                [ LunchMoney.insertTransactions token iq.queue (GotResponse >> toMsg)
                ]
            )

        Error _ previousTransactions ->
            let
                transactionsToInsert =
                    previousTransactions ++ iq.queue
            in
            ( InsertQueue
                { queue = []
                , processing = Loading transactionsToInsert
                }
            , Cmd.batch
                [ LunchMoney.insertTransactions token transactionsToInsert (GotResponse >> toMsg)
                ]
            )

        Loading _ ->
            ( InsertQueue iq, Cmd.none )
