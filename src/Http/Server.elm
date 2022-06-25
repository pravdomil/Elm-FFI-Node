module Http.Server exposing
    ( Server, close
    , Msg, init, update, subscriptions
    , PublicMsg(..), toPublicMsg
    )

{-|

@docs Server, close

@docs Msg, init, update, subscriptions

@docs PublicMsg, toPublicMsg

-}

import Console
import Http.Server.Internals
import JavaScript
import Json.Encode
import Process.Extra
import Task


type Server
    = Server Model


close : Cmd Msg
close =
    sendMessageToSelf PleaseClose



--


type alias Model =
    { options : Http.Server.Internals.ListenOptions
    , server : ServerState
    , status : Status
    }


type ServerState
    = NoServer
    | LoadingServer
    | ReadyServer Http.Server.Internals.Server



--


init : Http.Server.Internals.ListenOptions -> ( Server, Cmd Msg )
init options =
    Server (Model options NoServer Running) |> lifecycle



--


type Msg
    = GotServer (Result JavaScript.Error Http.Server.Internals.Server)
    | GotEvent Http.Server.Internals.Msg
    | PleaseClose
    | NoOperation


type PublicMsg
    = GotRequest Http.Server.Internals.Request


toPublicMsg : Msg -> Maybe PublicMsg
toPublicMsg a =
    case a of
        GotEvent (Http.Server.Internals.GotRequest b) ->
            Just (GotRequest b)

        _ ->
            Nothing


update : Msg -> Server -> ( Server, Cmd Msg )
update msg (Server a) =
    (case msg of
        GotServer b ->
            case b of
                Ok c ->
                    ( Server { a | server = ReadyServer c }
                    , Console.log "Server started."
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Err c ->
                    ( Server a
                    , Console.logError ("Cannot start server. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NoOperation)
                    )

        GotEvent b ->
            case b of
                Http.Server.Internals.ServerError c ->
                    ( Server a
                    , Console.logError ("Got server error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Http.Server.Internals.GotRequest _ ->
                    ( Server a
                    , Cmd.none
                    )

                Http.Server.Internals.RequestError c ->
                    ( Server a
                    , Console.logError ("Got request error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Http.Server.Internals.ResponseError c ->
                    ( Server a
                    , Console.logError ("Got response error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NoOperation)
                    )

        PleaseClose ->
            ( Server { a | status = Exiting }
            , Cmd.none
            )

        NoOperation ->
            ( Server a
            , Cmd.none
            )
    )
        |> (\( v, cmd ) ->
                let
                    ( v2, cmd2 ) =
                        lifecycle v
                in
                ( v2
                , Cmd.batch [ cmd, cmd2 ]
                )
           )


lifecycle : Server -> ( Server, Cmd Msg )
lifecycle (Server a) =
    case a.status of
        Running ->
            case a.server of
                NoServer ->
                    ( Server { a | server = LoadingServer }
                    , Http.Server.Internals.create a.options |> Task.attempt GotServer
                    )

                LoadingServer ->
                    ( Server a
                    , Cmd.none
                    )

                ReadyServer _ ->
                    ( Server a
                    , Cmd.none
                    )

        Exiting ->
            case a.server of
                NoServer ->
                    ( Server a
                    , Cmd.none
                    )

                LoadingServer ->
                    ( Server a
                    , Cmd.none
                    )

                ReadyServer b ->
                    ( Server { a | server = NoServer }
                    , Http.Server.Internals.close b
                        |> Task.attempt (\_ -> NoOperation)
                    )



--


subscriptions : Server -> Sub Msg
subscriptions _ =
    Http.Server.Internals.onMsg GotEvent



--


type Status
    = Running
    | Exiting



--


sendMessageToSelf : a -> Cmd a
sendMessageToSelf a =
    Task.succeed () |> Task.perform (\() -> a)
