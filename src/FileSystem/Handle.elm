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
        """
        (() => {
          var c = require('fs').constants
          return require('fs/promises').open(
            a.path,
            (a.mode & 3 ? c.O_RDWR : ((a.mode & 1 ? c.O_RDONLY : 0) | (a.mode & 2 ? c.O_WRONLY : 0))) |
            (a.mode & 4 ? c.O_APPEND : 0) |
            (a.mode & 8 ? c.O_CREAT : 0) |
            (a.mode & 16 ? c.O_TRUNC : 0)
          )
        })()
        """
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


write : String -> Handle -> Task.Task JavaScript.Error ()
write data (Handle a) =
    JavaScript.run
        "a.handle.writeFile(a.data)"
        (Json.Encode.object
            [ ( "handle", a )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed ())


truncate : Handle -> Task.Task JavaScript.Error ()
truncate (Handle a) =
    JavaScript.run
        "a.truncate()"
        a
        (Json.Decode.succeed ())


close : Handle -> Task.Task JavaScript.Error ()
close (Handle a) =
    JavaScript.run
        "a.close()"
        a
        (Json.Decode.succeed ())



--


type alias Mode =
    { read : ReadMode
    , write : WriteMode
    , create : Creation
    , truncate : Truncation
    }


type ReadMode
    = Read
    | NoRead


type WriteMode
    = Write
    | Append
    | NoWrite


type Creation
    = CreateIfNotExists
    | DoNotCreate


type Truncation
    = TruncateBeforeOpen
    | DoNotTruncate


modeToInt : Mode -> Int
modeToInt a =
    0
        |> (case a.read of
                Read ->
                    Bitwise.or 1

                NoRead ->
                    identity
           )
        |> (case a.write of
                Write ->
                    Bitwise.or 2

                Append ->
                    Bitwise.or 4

                NoWrite ->
                    identity
           )
        |> (case a.create of
                CreateIfNotExists ->
                    Bitwise.or 8

                DoNotCreate ->
                    identity
           )
        |> (case a.truncate of
                TruncateBeforeOpen ->
                    Bitwise.or 16

                DoNotTruncate ->
                    identity
           )
