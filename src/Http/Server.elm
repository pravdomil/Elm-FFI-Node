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
    sendMessageToSelf CloseRequested



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
    | ServerReceived (Result JavaScript.Error Http.Server.Internals.Server)
    | ServerMessageReceived Http.Server.Internals.Msg
    | CloseRequested


type PublicMsg
    = GotRequest Http.Server.Internals.Request


toPublicMsg : Msg -> Maybe PublicMsg
toPublicMsg a =
    case a of
        ServerMessageReceived (Http.Server.Internals.RequestReceived b) ->
            Just (GotRequest b)

        _ ->
            Nothing


update : Msg -> Server -> ( Server, Cmd Msg )
update msg (Server model) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        ServerReceived b ->
            case b of
                Ok c ->
                    ( { model | server = ReadyServer c }
                    , Console.log "Server started."
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Err c ->
                    ( model
                    , Console.logError ("Cannot start server. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NothingHappened)
                    )

        ServerMessageReceived b ->
            case b of
                Http.Server.Internals.ServerError c ->
                    ( model
                    , Console.logError ("Got server error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.andThen (\_ -> Process.Extra.exit 1)
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.RequestError c ->
                    ( model
                    , Console.logError ("Got request error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.ResponseError c ->
                    ( model
                    , Console.logError ("Got response error. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                        |> Task.attempt (\_ -> NothingHappened)
                    )

                Http.Server.Internals.RequestReceived _ ->
                    Platform.Extra.noOperation model

        CloseRequested ->
            ( { model | state = Exiting }
            , Cmd.none
            )
    )
        |> Tuple.mapFirst Server


lifecycle : Server -> ( Server, Cmd Msg )
lifecycle (Server model) =
    case model.state of
        Running ->
            case model.server of
                NoServer ->
                    ( Server { model | server = LoadingServer }
                    , Http.Server.Internals.create model.options |> Task.attempt ServerReceived
                    )

                LoadingServer ->
                    Platform.Extra.noOperation (Server model)

                ReadyServer _ ->
                    Platform.Extra.noOperation (Server model)

        Exiting ->
            case model.server of
                NoServer ->
                    Platform.Extra.noOperation (Server model)

                LoadingServer ->
                    Platform.Extra.noOperation (Server model)

                ReadyServer b ->
                    ( Server { model | server = NoServer }
                    , Http.Server.Internals.close b
                        |> Task.attempt (\_ -> NothingHappened)
                    )



--


subscriptions : Server -> Sub Msg
subscriptions _ =
    Http.Server.Internals.onMsg ServerMessageReceived



--


type State
    = Running
    | Exiting



--


sendMessageToSelf : a -> Cmd a
sendMessageToSelf a =
    Task.succeed () |> Task.perform (\() -> a)
