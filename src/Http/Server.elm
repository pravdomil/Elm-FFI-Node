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

import Http.Server.Internals
import JavaScript
import LogMessage
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
                    , LogMessage.log LogMessage.Info (LogMessage.Name "Server started.") (LogMessage.Details "") []
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Err c ->
                    ( Server a
                    , LogMessage.log LogMessage.Error (LogMessage.Name "Cannot start server.") (LogMessage.JavaScriptError c) []
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NoOperation)
                    )

        GotEvent b ->
            case b of
                Http.Server.Internals.ServerError c ->
                    ( Server a
                    , LogMessage.log LogMessage.Error (LogMessage.Name "Got server error.") (LogMessage.JavaScriptError c) []
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Http.Server.Internals.GotRequest _ ->
                    ( Server a
                    , Cmd.none
                    )

                Http.Server.Internals.RequestError c ->
                    ( Server a
                    , LogMessage.log LogMessage.Warning (LogMessage.Name "Got request error.") (LogMessage.JavaScriptError c) []
                        |> Task.attempt (\_ -> NoOperation)
                    )

                Http.Server.Internals.ResponseError c ->
                    ( Server a
                    , LogMessage.log LogMessage.Warning (LogMessage.Name "Got response error.") (LogMessage.JavaScriptError c) []
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
