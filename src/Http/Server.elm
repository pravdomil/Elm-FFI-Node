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
import Platform.Extra
import Process.Extra
import Task


type Server
    = Server Model


close : Cmd Msg
close =
    sendMessageToSelf PleaseClose



--


type alias Model =
    { options : Http.Server.Internals.Options
    , server : ServerState
    , state : State
    }


type ServerState
    = NoServer
    | LoadingServer
    | ReadyServer Http.Server.Internals.Server



--


init : Http.Server.Internals.Options -> ( Server, Cmd Msg )
init options =
    Server (Model options NoServer Running) |> lifecycle



--


type Msg
    = NothingHappened
    | GotServer (Result JavaScript.Error Http.Server.Internals.Server)
    | GotEvent Http.Server.Internals.Msg
    | PleaseClose


type PublicMsg
    = GotRequest Http.Server.Internals.Request


toPublicMsg : Msg -> Maybe PublicMsg
toPublicMsg a =
    case a of
        GotEvent (Http.Server.Internals.RequestReceived b) ->
            Just (GotRequest b)

        _ ->
            Nothing


update : Msg -> Server -> ( Server, Cmd Msg )
update msg (Server a) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation (Server a)

        GotServer b ->
            case b of
                Ok c ->
                    ( Server { a | server = ReadyServer c }
                    , Console.log "Server started."
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Err c ->
                    ( Server a
                    , Console.logError ("Cannot start server. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NothingHappened)
                    )

        GotEvent b ->
            case b of
                Http.Server.Internals.ServerError c ->
                    ( Server a
                    , Console.logError ("Got server error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.RequestError c ->
                    ( Server a
                    , Console.logError ("Got request error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.ResponseError c ->
                    ( Server a
                    , Console.logError ("Got response error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.RequestReceived _ ->
                    Platform.Extra.noOperation (Server a)

        PleaseClose ->
            ( Server { a | state = Exiting }
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
    case a.state of
        Running ->
            case a.server of
                NoServer ->
                    ( Server { a | server = LoadingServer }
                    , Http.Server.Internals.create a.options |> Task.attempt GotServer
                    )

                LoadingServer ->
                    Platform.Extra.noOperation (Server a)

                ReadyServer _ ->
                    Platform.Extra.noOperation (Server a)

        Exiting ->
            case a.server of
                NoServer ->
                    Platform.Extra.noOperation (Server a)

                LoadingServer ->
                    Platform.Extra.noOperation (Server a)

                ReadyServer b ->
                    ( Server { a | server = NoServer }
                    , Http.Server.Internals.close b
                        |> Task.attempt (\_ -> NothingHappened)
                    )



--


subscriptions : Server -> Sub Msg
subscriptions _ =
    Http.Server.Internals.onMsg GotEvent



--


type State
    = Running
    | Exiting



--


sendMessageToSelf : a -> Cmd a
sendMessageToSelf a =
    Task.succeed () |> Task.perform (\() -> a)
