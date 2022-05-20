module Console exposing (..)

{-| <https://nodejs.org/api/console.html>
-}

import JavaScript
import Json.Decode
import Json.Encode
import Task


log : String -> Task.Task JavaScript.Error String
log a =
    JavaScript.run "console.log(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed a)


logError : String -> Task.Task JavaScript.Error String
logError a =
    JavaScript.run "console.error(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed a)


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
