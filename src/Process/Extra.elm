module Process.Extra exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task


exit : Int -> Task.Task JavaScript.Error ()
exit code =
    JavaScript.run
        "process.exit(a)"
        (Json.Encode.int code)
        (Json.Decode.succeed ())


softExit : Task.Task JavaScript.Error ()
softExit =
    JavaScript.run
        "process.kill(process.pid, 'SIGTERM')"
        Json.Encode.null
        (Json.Decode.succeed ())


onInterruptAndTerminationSignal : msg -> Cmd msg
onInterruptAndTerminationSignal msg =
    JavaScript.run
        "new Promise(resolve => { process.once('SIGINT', resolve); process.once('SIGTERM', resolve) })"
        Json.Encode.null
        (Json.Decode.succeed ())
        |> Task.attempt (\_ -> msg)


onBeforeExit : msg -> Cmd msg
onBeforeExit msg =
    JavaScript.run
        "new Promise(resolve => { process.once('beforeExit', resolve) })"
        Json.Encode.null
        (Json.Decode.succeed ())
        |> Task.attempt (\_ -> msg)
