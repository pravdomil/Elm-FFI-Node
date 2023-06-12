module Process.Extra exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task


softExit : Task.Task JavaScript.Error ()
softExit =
    JavaScript.run
        "process.kill(process.pid, 'SIGTERM')"
        Json.Encode.null
        (Json.Decode.succeed ())


hardExit : Int -> Task.Task JavaScript.Error ()
hardExit code =
    JavaScript.run
        "process.exit(a)"
        (Json.Encode.int code)
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



--


spawn : String -> List String -> Task.Task JavaScript.Error String
spawn a args =
    JavaScript.run """
        new Promise((resolve, reject) => {
            function err(code, msg) {
                var e = new Error(msg)
                e.code = code
                return e
            }
    
            var b = require('child_process').spawn(a[0], a[1]);
            var stdout = '';
            var stderr = '';
            b.on('error', reject)
            b.on('close', b => { b ? reject(err('ENONZERO', stderr)) : resolve(stdout) })
            b.stdout.on('data', b => { stdout += b })
            b.stderr.on('data', b => { stderr += b })
            onCancel(() => b.kill())
        })
        """
        (Json.Encode.list identity
            [ Json.Encode.string a
            , Json.Encode.list Json.Encode.string args
            ]
        )
        Json.Decode.string
