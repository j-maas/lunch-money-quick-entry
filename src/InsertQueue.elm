module InsertQueue exposing (ClientError(..), Error(..), InsertQueue, Msg, Processing(..), ServerError(..), codecInsertQueue, empty, insert, isEmpty, processQueue, processing, size, update)

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


isEmpty : InsertQueue -> Bool
isEmpty (InsertQueue iq) =
    case iq.processing of
        Loading t ->
            List.isEmpty t

        _ ->
            List.isEmpty iq.queue


size : InsertQueue -> Int
size iq =
    getAllUnsent iq
        |> List.length


codecInsertQueue : Codec InsertQueue
codecInsertQueue =
    Codec.list LunchMoney.codecTransaction
        |> Codec.map
            (\transactions ->
                InsertQueue
                    { queue = transactions
                    , processing = Idle
                    }
            )
            (\iq ->
                getAllUnsent iq
            )


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
                            getAllUnsent (InsertQueue iq)

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


getAllUnsent : InsertQueue -> List Transaction
getAllUnsent (InsertQueue iq) =
    let
        loading =
            case iq.processing of
                Idle ->
                    []

                Loading t ->
                    t

                Failed _ ->
                    []
    in
    loading ++ iq.queue


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
