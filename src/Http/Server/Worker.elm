module Http.Server.Worker exposing (Model, Msg, init, update, subscriptions)

{-|

@docs Model, Msg, init, update, subscriptions

-}

import Codec
import Console
import Http.Server
import JavaScript
import LogMessage
import Platform.Extra
import Process
import Process.Extra
import Task
import Task.Extra


type alias Model =
    { server : Result Error Http.Server.Server
    }



--


type Error
    = NoServer
    | Loading
    | Closing
    | CreateError JavaScript.Error
    | CloseError JavaScript.Error



--


init : Http.Server.Options -> ( Model, Cmd Msg )
init options =
    ( Model
        (Err NoServer)
    , Cmd.none
    )
        |> Platform.Extra.andThen (createServer options)



--


type Msg
    = NothingHappened
    | ServerCreated (Result JavaScript.Error Http.Server.Server)
    | MessageReceived Http.Server.Msg
    | CloseRequested
    | ServerClosed (Result JavaScript.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                        "HTTP Server"
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
                        "HTTP Server"
                        "Cannot start server."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model | server = Err (CreateError b) }
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.softExit)
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
                        "HTTP Server"
                        "Server error."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.softExit)
                |> Task.attempt (\_ -> NothingHappened)
            )

        Http.Server.RequestError b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Warning
                        "HTTP Server"
                        "Request error."
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
                        "HTTP Server"
                        "Response error."
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

        Err b ->
            case b of
                Loading ->
                    ( model
                    , Process.sleep 100
                        |> Task.perform (\() -> CloseRequested)
                    )

                _ ->
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
                        "HTTP Server"
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
                        "HTTP Server"
                        "Cannot close server."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model | server = Err (CloseError b) }
            , logMessage message
                |> Task.attempt (\_ -> NothingHappened)
            )



--


subscriptions : Model -> Sub Msg
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
