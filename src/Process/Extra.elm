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
