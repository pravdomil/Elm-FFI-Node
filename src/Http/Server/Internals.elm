port module Http.Server.Internals exposing
    ( Server, create, close
    , ListenOptions, emptyListenOptions
    , Msg(..), onMsg
    , Request, requestMethod, requestUrl, requestParts, Part(..), File
    , Response, respond
    )

{-|

@docs Server, create, close

@docs ListenOptions, emptyListenOptions

@docs Msg, onMsg

@docs Request, requestMethod, requestUrl, requestParts, Part, File

@docs Response, respond

-}

import Codec
import Dict
import FileSystem
import JavaScript
import Json.Decode
import Json.Encode
import Task


{-| <https://nodejs.org/api/http.html#class-httpserver>
-}
type Server
    = Server Json.Decode.Value


{-| <https://nodejs.org/api/http.html#httpcreateserveroptions-requestlistener>
-}
create : ListenOptions -> Task.Task JavaScript.Error Server
create options =
    JavaScript.run """
    (() => {
        var b = require('http').createServer()
        var c = require('formidable')()
        b.on('error', e => { scope.ports.httpServerInternals.send({ $: 0, a: e }) })
        b.on('request', (req, res) => {
          c.parse(req, (e, fields, files) => {
            scope.ports.httpServerInternals.send(e ? { $: 1, a: e } : { $: 3, a: { req, res, fields, files } })
          })
          res.on('error', e => { scope.ports.httpServerInternals.send({ $: 2, a: e }) })
        })
        b.listen(a)
        return b
    })()
    """
        (options |> Codec.encodeToValue listenOptionsCodec)
        (Json.Decode.value |> Json.Decode.map Server)


close : Server -> Task.Task JavaScript.Error ()
close (Server a) =
    JavaScript.run "new Promise((resolve, reject) => { a.close(b => { b ? reject(b) : resolve() }) })"
        a
        (Json.Decode.succeed ())



--


port httpServerInternals : (Json.Decode.Value -> msg) -> Sub msg


type Msg
    = ServerError JavaScript.Error
    | RequestError JavaScript.Error
    | ResponseError JavaScript.Error
    | GotRequest Request


onMsg : (Msg -> msg) -> Sub msg
onMsg toMsg =
    let
        toEvent : Json.Decode.Value -> Msg
        toEvent b =
            b
                |> Json.Decode.decodeValue
                    (Json.Decode.field "$" Json.Decode.int
                        |> Json.Decode.andThen
                            (\v ->
                                case v of
                                    0 ->
                                        Json.Decode.map ServerError
                                            (Json.Decode.field "a" Json.Decode.value
                                                |> Json.Decode.map JavaScript.decodeError
                                            )

                                    1 ->
                                        Json.Decode.map RequestError
                                            (Json.Decode.field "a" Json.Decode.value
                                                |> Json.Decode.map JavaScript.decodeError
                                            )

                                    2 ->
                                        Json.Decode.map ResponseError
                                            (Json.Decode.field "a" Json.Decode.value
                                                |> Json.Decode.map JavaScript.decodeError
                                            )

                                    3 ->
                                        let
                                            fileDecoder : Json.Decode.Decoder File
                                            fileDecoder =
                                                Json.Decode.map4 File
                                                    (Json.Decode.field "filepath" (Json.Decode.string |> Json.Decode.map FileSystem.Path))
                                                    (Json.Decode.field "originalFilename" Json.Decode.string)
                                                    (Json.Decode.field "mimetype" Json.Decode.string)
                                                    (Json.Decode.field "size" Json.Decode.int)
                                        in
                                        Json.Decode.field "a"
                                            (Json.Decode.map3 (\v1 v2 v3 -> GotRequest (Request v1 v2 v3))
                                                (Json.Decode.field "req" Json.Decode.value)
                                                (Json.Decode.field "res" Json.Decode.value)
                                                (Json.Decode.map2 Dict.union
                                                    (Json.Decode.field "fields"
                                                        (Json.Decode.dict
                                                            (Json.Decode.string |> Json.Decode.map StringPart)
                                                        )
                                                    )
                                                    (Json.Decode.field "files"
                                                        (Json.Decode.dict
                                                            (fileDecoder |> Json.Decode.map FilePart)
                                                        )
                                                    )
                                                )
                                            )

                                    _ ->
                                        Json.Decode.fail "Cannot decode message."
                            )
                    )
                |> (\v ->
                        case v of
                            Ok v2 ->
                                v2

                            Err v2 ->
                                ServerError (JavaScript.DecodeError v2)
                   )
    in
    httpServerInternals (toEvent >> toMsg)



--


type alias ListenOptions =
    { port_ : Maybe Int
    , host : Maybe String
    , path : Maybe String
    , backlog : Maybe Int
    , exclusive : Maybe Bool
    , readableAll : Maybe Bool
    , writableAll : Maybe Bool
    , ipv6Only : Maybe Bool
    }


emptyListenOptions : ListenOptions
emptyListenOptions =
    ListenOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


listenOptionsCodec : Codec.Codec ListenOptions
listenOptionsCodec =
    Codec.object ListenOptions
        |> Codec.maybeField "port" .port_ Codec.int
        |> Codec.maybeField "host" .host Codec.string
        |> Codec.maybeField "path" .path Codec.string
        |> Codec.maybeField "backlog" .backlog Codec.int
        |> Codec.maybeField "exclusive" .exclusive Codec.bool
        |> Codec.maybeField "readableAll" .readableAll Codec.bool
        |> Codec.maybeField "writableAll" .writableAll Codec.bool
        |> Codec.maybeField "ipv6Only" .ipv6Only Codec.bool
        |> Codec.buildObject



--


{-| <https://nodejs.org/api/http.html#class-httpincomingmessage>
-}
type Request
    = Request Json.Decode.Value Json.Decode.Value (Dict.Dict String Part)


requestMethod : Request -> String
requestMethod (Request a _ _) =
    a
        |> Json.Decode.decodeValue (Json.Decode.field "method" Json.Decode.string)
        |> Result.withDefault ""


requestUrl : Request -> String
requestUrl (Request a _ _) =
    a
        |> Json.Decode.decodeValue (Json.Decode.field "url" Json.Decode.string)
        |> Result.withDefault ""


requestParts : Request -> Dict.Dict String Part
requestParts (Request _ _ a) =
    a



--


type Part
    = StringPart String
    | FilePart File



--


type alias File =
    { path : FileSystem.Path
    , name : String
    , mime : String
    , size : Int
    }



--


type alias Response =
    { statusCode : Int
    , headers : List ( String, String )
    , data : String
    }


respond : Response -> Request -> Task.Task JavaScript.Error ()
respond response (Request _ res _) =
    JavaScript.run "new Promise((resolve, reject) => { a.res.writeHead(a.a, a.b); a.res.end(a.c, b => { b ? reject(b) : resolve() }) })"
        (Json.Encode.object
            [ ( "res", res )
            , ( "a", response.statusCode |> Json.Encode.int )
            , ( "b", response.headers |> List.foldl (\( v1, v2 ) acc -> v1 :: v2 :: acc) [] |> Json.Encode.list Json.Encode.string )
            , ( "c", response.data |> Json.Encode.string )
            ]
        )
        (Json.Decode.succeed ())
