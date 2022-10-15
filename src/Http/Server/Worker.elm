module Http.Server.Worker exposing
    ( Worker, close
    , Msg, init, update, subscriptions
    , PublicMsg(..), toPublicMsg
    )

{-|

@docs Worker, close

@docs Msg, init, update, subscriptions

@docs PublicMsg, toPublicMsg

-}

import Codec
import Console
import Http.Server
import JavaScript
import LogMessage
import Platform.Extra
import Process.Extra
import Task
import Task.Extra


type Worker
    = Worker Model


close : Worker -> ( Worker, Cmd Msg )
close a =
    update CloseRequested a



--


type alias Model =
    { server : Result Error Http.Server.Server
    }



--


type Error
    = NoServer
    | Loading
    | Closing
    | JavaScriptError JavaScript.Error



--


init : Http.Server.Options -> ( Worker, Cmd Msg )
init options =
    ( Model
        (Err NoServer)
    , Cmd.none
    )
        |> Platform.Extra.andThen (createServer options)
        |> Tuple.mapFirst Worker



--


type Msg
    = NothingHappened
    | ServerCreated (Result JavaScript.Error Http.Server.Server)
    | MessageReceived Http.Server.Msg
    | CloseRequested
    | ServerClosed (Result JavaScript.Error ())


update : Msg -> Worker -> ( Worker, Cmd Msg )
update msg (Worker model) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        ServerCreated b ->
            serverCreated b model

        MessageReceived b ->
            messageReceived b model

        CloseRequested ->
            closeServer model

        ServerClosed b ->
            serverClosed b model
    )
        |> Tuple.mapFirst Worker



--


type PublicMsg
    = RequestReceived Http.Server.Request


toPublicMsg : Msg -> Maybe PublicMsg
toPublicMsg a =
    case a of
        MessageReceived (Http.Server.RequestReceived b) ->
            Just (RequestReceived b)

        _ ->
            Nothing



--


createServer : Http.Server.Options -> Model -> ( Model, Cmd Msg )
createServer options model =
    case model.server of
        Err NoServer ->
            ( { model | server = Err Loading }
            , Http.Server.create options
                |> Task.attempt ServerCreated
            )

        _ ->
            Platform.Extra.noOperation model


serverCreated : Result JavaScript.Error Http.Server.Server -> Model -> ( Model, Cmd Msg )
serverCreated result model =
    case result of
        Ok b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Info
                        "Server started."
                        Nothing
            in
            ( { model | server = Ok b }
            , logMessage message
                |> Task.attempt (\_ -> NothingHappened)
            )

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot start server."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model | server = Err (JavaScriptError b) }
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.exit 1)
                |> Task.attempt (\_ -> NothingHappened)
            )


messageReceived : Http.Server.Msg -> Model -> ( Model, Cmd Msg )
messageReceived msg model =
    case msg of
        Http.Server.ServerError b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Got server error."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.exit 1)
                |> Task.attempt (\_ -> NothingHappened)
            )

        Http.Server.RequestError b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Warning
                        "Got request error."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , logMessage message
                |> Task.attempt (\_ -> NothingHappened)
            )

        Http.Server.ResponseError b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Warning
                        "Got response error."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , logMessage message
                |> Task.attempt (\_ -> NothingHappened)
            )

        Http.Server.RequestReceived _ ->
            Platform.Extra.noOperation model


closeServer : Model -> ( Model, Cmd Msg )
closeServer model =
    case model.server of
        Ok b ->
            ( { model | server = Err Closing }
            , Http.Server.close b
                |> Task.attempt ServerClosed
            )

        Err _ ->
            Platform.Extra.noOperation model


serverClosed : Result JavaScript.Error () -> Model -> ( Model, Cmd Msg )
serverClosed result model =
    case result of
        Ok () ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Info
                        "Server closed."
                        Nothing
            in
            ( { model | server = Err NoServer }
            , logMessage message
                |> Task.attempt (\_ -> NothingHappened)
            )

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot close server."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model | server = Err (JavaScriptError b) }
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.exit 1)
                |> Task.attempt (\_ -> NothingHappened)
            )



--


subscriptions : Worker -> Sub Msg
subscriptions _ =
    Http.Server.onMsg MessageReceived



--


logMessage : LogMessage.LogMessage -> Task.Task JavaScript.Error ()
logMessage a =
    let
        fn : String -> Task.Task JavaScript.Error ()
        fn =
            case a.type_ of
                LogMessage.Info ->
                    Console.logInfo

                LogMessage.Warning ->
                    Console.logWarning

                LogMessage.Error ->
                    Console.logError
    in
    fn (Codec.encodeToString 0 LogMessage.codec a)
