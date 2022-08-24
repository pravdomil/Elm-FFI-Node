module Console exposing (..)

{-| <https://nodejs.org/api/console.html>
-}

import JavaScript
import Json.Decode
import Json.Encode
import Task


log : String -> Task.Task JavaScript.Error ()
log a =
    JavaScript.run "console.log(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logInfo : String -> Task.Task JavaScript.Error ()
logInfo a =
    JavaScript.run "console.info(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logWarning : String -> Task.Task JavaScript.Error ()
logWarning a =
    JavaScript.run "console.warn(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logError : String -> Task.Task JavaScript.Error ()
logError a =
    JavaScript.run "console.error(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


time : String -> Task.Task JavaScript.Error ()
time a =
    JavaScript.run "console.time(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


timeEnd : String -> Task.Task JavaScript.Error ()
timeEnd a =
    JavaScript.run "console.timeEnd(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


timeTask : (JavaScript.Error -> x) -> String -> Task.Task x a -> Task.Task x a
timeTask toError label a =
    time label
        |> Task.mapError toError
        |> Task.andThen
            (\_ ->
                a
                    |> taskAndThenWithResult
                        (\v ->
                            timeEnd label
                                |> Task.mapError toError
                                |> Task.andThen
                                    (\_ ->
                                        case v of
                                            Ok v2 ->
                                                Task.succeed v2

                                            Err v2 ->
                                                Task.fail v2
                                    )
                        )
            )



--


taskAndThenWithResult : (Result x a -> Task.Task y b) -> Task.Task x a -> Task.Task y b
taskAndThenWithResult toTask a =
    a
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.andThen toTask
