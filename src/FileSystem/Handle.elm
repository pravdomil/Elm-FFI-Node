module FileSystem.Handle exposing (..)

import Bitwise
import FileSystem
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
        "a.readFile({ encoding: 'utf8' })"
        a
        Json.Decode.string


write : String -> Handle -> Task.Task JavaScript.Error Handle
write data (Handle a) =
    JavaScript.run
        "a.handle.writeFile(a.data)"
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
    , create : Creation
    , truncate : Truncation
    , append : Append
    }


type ModeType
    = Read
    | Write
    | ReadAndWrite


type Creation
    = CreateIfNotExists
    | DoNotCreate


type Truncation
    = TruncateBeforeOpen
    | DoNotTruncate


type Append
    = Append
    | DoNotAppend


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
        |> (case a.append of
                Append ->
                    Bitwise.or 8

                DoNotAppend ->
                    identity
           )
        |> (case a.create of
                CreateIfNotExists ->
                    Bitwise.or 512

                DoNotCreate ->
                    identity
           )
        |> (case a.truncate of
                TruncateBeforeOpen ->
                    Bitwise.or 1024

                DoNotTruncate ->
                    identity
           )
