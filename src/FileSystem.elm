module FileSystem exposing (..)

{-| <https://nodejs.org/api/fs.html>
-}

import JavaScript
import Json.Decode
import Json.Encode
import Task


type Path
    = Path String



--


read : Path -> Task.Task JavaScript.Error String
read (Path a) =
    JavaScript.run
        "require('fs/promises').readFile(a, 'utf-8')"
        (Json.Encode.string a)
        Json.Decode.string


write : Path -> String -> Task.Task JavaScript.Error ()
write (Path a) data =
    JavaScript.run
        "require('fs/promises').writeFile(a.path, a.data)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string a )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed ())


append : Path -> String -> Task.Task JavaScript.Error ()
append (Path a) data =
    JavaScript.run
        "require('fs/promises').appendFile(a.path, a.data)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string a )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed ())


delete : Path -> Task.Task JavaScript.Error ()
delete (Path a) =
    JavaScript.run
        "require('fs/promises').unlink(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())
