module Worker exposing (commandLine, commandLineWithStdin)

import JavaScript
import Json.Decode
import Json.Encode
import Process.Extra
import Task
import Task.Extra


commandLine : (List String -> Task.Task String String) -> Program () () ()
commandLine fn =
    helper
        (readArgs
            |> Task.mapError JavaScript.errorToString
            |> Task.andThen fn
        )


commandLineWithStdin : ({ arguments : List String, stdin : String } -> Task.Task String String) -> Program () () ()
commandLineWithStdin fn =
    helper
        (Task.map2 (\x x2 -> { arguments = x, stdin = x2 })
            readArgs
            readStdin
            |> Task.mapError JavaScript.errorToString
            |> Task.andThen fn
        )



--


helper : Task.Task String String -> Program () () ()
helper a =
    let
        cmd : Cmd ()
        cmd =
            a
                |> Task.Extra.andAlwaysThen
                    (\x ->
                        case x of
                            Ok x2 ->
                                writeStdout x2
                                    |> Task.andThen (\_ -> Process.Extra.hardExit 0)
                                    |> Task.mapError JavaScript.errorToString

                            Err x2 ->
                                writeStderr x2
                                    |> Task.andThen (\_ -> Process.Extra.hardExit 1)
                                    |> Task.mapError JavaScript.errorToString
                    )
                |> Task.attempt (\_ -> ())
    in
    Platform.worker
        { init = \_ -> ( (), cmd )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }



--


readArgs : Task.Task JavaScript.Error (List String)
readArgs =
    JavaScript.run "process.argv"
        Json.Encode.null
        (Json.Decode.list Json.Decode.string)


readStdin : Task.Task JavaScript.Error String
readStdin =
    JavaScript.run "require('fs').readFileSync(0, 'utf8')"
        Json.Encode.null
        Json.Decode.string



--


writeStdout : String -> Task.Task JavaScript.Error ()
writeStdout a =
    JavaScript.run "process.stdout.write(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


writeStderr : String -> Task.Task JavaScript.Error ()
writeStderr a =
    JavaScript.run "process.stderr.write(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())
