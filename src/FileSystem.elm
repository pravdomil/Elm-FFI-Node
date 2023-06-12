module FileSystem exposing (Path, append, chmod, delete, pathToString, read, rename, stringToPath, write)

{-| <https://nodejs.org/api/fs.html>
-}

import JavaScript
import Json.Decode
import Json.Encode
import Task


type Path
    = Path String


stringToPath : String -> Path
stringToPath =
    Path


pathToString : Path -> String
pathToString (Path a) =
    a



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


chmod : Path -> Int -> Task.Task JavaScript.Error ()
chmod (Path a) mode =
    JavaScript.run "require('fs/promises').chmod(a.path, a.mode)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string a )
            , ( "mode", Json.Encode.int mode )
            ]
        )
        (Json.Decode.succeed ())


rename : Path -> Path -> Task.Task JavaScript.Error ()
rename (Path from) (Path to) =
    JavaScript.run
        "require('fs/promises').rename(a.a, a.b)"
        (Json.Encode.object
            [ ( "a", Json.Encode.string from )
            , ( "b", Json.Encode.string to )
            ]
        )
        (Json.Decode.succeed ())


delete : Path -> Task.Task JavaScript.Error ()
delete (Path a) =
    JavaScript.run
        "require('fs/promises').unlink(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())



--


createDirectory : Path -> Task.Task JavaScript.Error ()
createDirectory (Path a) =
    JavaScript.run
        "fs.promises.mkdir(a, { recursive: true })"
        (Json.Encode.string a)
        (Json.Decode.succeed ())
