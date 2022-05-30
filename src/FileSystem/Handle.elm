module FileSystem.Handle exposing (..)

import Bitwise
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Decode
import Json.Encode
import Task


type Handle
    = Handle Json.Decode.Value


open : Mode -> FileSystem.Path -> Task.Task JavaScript.Error Handle
open mode (FileSystem.Path a) =
    JavaScript.run
        "require('fs/promises').open(a.path, a.mode)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string a )
            , ( "mode", Json.Encode.int (modeToInt mode) )
            ]
        )
        (Json.Decode.value |> Json.Decode.map Handle)


read : Handle -> Task.Task JavaScript.Error String
read (Handle a) =
    JavaScript.run
        "a.read()"
        a
        Json.Decode.string


write : String -> Handle -> Task.Task JavaScript.Error Handle
write data (Handle a) =
    JavaScript.run
        "(() => { var b = Buffer.from(a.data); return a.handle.write(b).then(c => b.length === c.bytesWritten ? undefined : Promise.reject('Write failed call ferror().')); })()"
        (Json.Encode.object
            [ ( "handle", a )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed (Handle a))


truncate : Handle -> Task.Task JavaScript.Error Handle
truncate (Handle a) =
    JavaScript.run
        "a.truncate()"
        a
        (Json.Decode.succeed (Handle a))


close : Handle -> Task.Task JavaScript.Error ()
close (Handle a) =
    JavaScript.run
        "a.close()"
        a
        (Json.Decode.succeed ())



--


type alias Mode =
    { type_ : ModeType
    , create : Bool
    , truncate : Bool
    , append : Bool
    }


type ModeType
    = Read
    | Write
    | ReadAndWrite


modeToInt : Mode -> Int
modeToInt a =
    (case a.type_ of
        Read ->
            0

        Write ->
            1

        ReadAndWrite ->
            2
    )
        |> (if a.append then
                Bitwise.or 8

            else
                identity
           )
        |> (if a.create then
                Bitwise.or 512

            else
                identity
           )
        |> (if a.truncate then
                Bitwise.or 1024

            else
                identity
           )
