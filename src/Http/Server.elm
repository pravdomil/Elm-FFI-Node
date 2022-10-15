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

import Codec
import Console
import Http.Server.Internals
import JavaScript
import LogMessage
import Platform.Extra
import Process.Extra
import Task
import Task.Extra


type Server
    = Server Model


close : Server -> ( Server, Cmd Msg )
close a =
    update CloseRequested a



--


type alias Model =
    { server : Result Error Http.Server.Internals.Server
    }



--


type Error
    = NoServer
    | Loading
    | Closing
    | JavaScriptError JavaScript.Error



--


init : Http.Server.Internals.Options -> ( Server, Cmd Msg )
init options =
    ( Model
        (Err NoServer)
    , Cmd.none
    )
        |> Platform.Extra.andThen (createServer options)
        |> Tuple.mapFirst Server



--


type Msg
    = NothingHappened
    | ServerCreated (Result JavaScript.Error Http.Server.Internals.Server)
    | MessageReceived Http.Server.Internals.Msg
    | CloseRequested
    | ServerClosed (Result JavaScript.Error ())


update : Msg -> Server -> ( Server, Cmd Msg )
update msg (Server model) =
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
        |> Tuple.mapFirst Server



--


type PublicMsg
    = RequestReceived Http.Server.Internals.Request


toPublicMsg : Msg -> Maybe PublicMsg
toPublicMsg a =
    case a of
        MessageReceived (Http.Server.Internals.RequestReceived b) ->
            Just (RequestReceived b)

        _ ->
            Nothing



--


createServer : Http.Server.Internals.Options -> Model -> ( Model, Cmd Msg )
createServer options model =
    case model.server of
        Err NoServer ->
            ( { model | server = Err Loading }
            , Http.Server.Internals.create options
                |> Task.attempt ServerCreated
            )

        _ ->
            Platform.Extra.noOperation model


serverCreated : Result JavaScript.Error Http.Server.Internals.Server -> Model -> ( Model, Cmd Msg )
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


messageReceived : Http.Server.Internals.Msg -> Model -> ( Model, Cmd Msg )
messageReceived msg model =
    case msg of
        Http.Server.Internals.ServerError b ->
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

        Http.Server.Internals.RequestError b ->
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

        Http.Server.Internals.ResponseError b ->
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

        Http.Server.Internals.RequestReceived _ ->
            Platform.Extra.noOperation model


closeServer : Model -> ( Model, Cmd Msg )
closeServer model =
    case model.server of
        Ok b ->
            ( { model | server = Err Closing }
            , Http.Server.Internals.close b
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


subscriptions : Server -> Sub Msg
subscriptions _ =
    Http.Server.Internals.onMsg MessageReceived



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
