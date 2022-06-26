port module Http.Server.Internals exposing
    ( Server, create, close
    , Options, emptyOptions, optionsCodec
    , Msg(..), onMsg
    , Request, RequestResource, Part(..), File, requestCodec, partCodec, fileCodec
    , Response, respond, responseCodec
    )

{-|

@docs Server, create, close

@docs Options, emptyOptions, optionsCodec

@docs Msg, onMsg

@docs Request, RequestResource, Part, File, requestCodec, partCodec, fileCodec

@docs Response, respond, responseCodec

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
create : Options -> Task.Task JavaScript.Error Server
create options =
    let
        create_ : Task.Task JavaScript.Error Server
        create_ =
            JavaScript.run """
            import('formidable')
              .then(c => {
                var b = require('http').createServer()
                b.once('listening', () => { if (a.path) require('fs/promises').chmod(a.path, 0o775).then(() => {}, () => {}) })
                b.on('error', e => { scope.ports.httpServerInternals.send({ $: 0, a: e }) })
                b.on('request', (req, res) => {
                  res.on('error', e => { scope.ports.httpServerInternals.send({ $: 2, a: e }) })
                  c.default().parse(req, (e, fields, files) => {
                    scope.ports.httpServerInternals.send(e ? { $: 1, a: e } : { $: 3, a: { req, res, fields, files } })
                  })
                })
                b.listen(a)
                return b
              })
            """
                (options |> Codec.encodeToValue optionsCodec)
                (Json.Decode.value |> Json.Decode.map Server)

        clearSocket : Task.Task x ()
        clearSocket =
            case options.path of
                Just b ->
                    FileSystem.delete (FileSystem.Path b)
                        |> Task.onError (\_ -> Task.succeed ())

                Nothing ->
                    Task.succeed ()
    in
    clearSocket
        |> Task.andThen (\() -> create_)


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
        fileDecoder : Json.Decode.Decoder File
        fileDecoder =
            Json.Decode.map4 File
                (Json.Decode.field "filepath" (Json.Decode.string |> Json.Decode.map FileSystem.Path))
                (Json.Decode.field "originalFilename" Json.Decode.string)
                (Json.Decode.field "mimetype" Json.Decode.string)
                (Json.Decode.field "size" Json.Decode.int)

        requestDecoder : Json.Decode.Decoder Request
        requestDecoder =
            Json.Decode.map6 Request
                (Json.Decode.map2 RequestResource
                    (Json.Decode.field "req" Json.Decode.value)
                    (Json.Decode.field "res" Json.Decode.value)
                    |> Json.Decode.map Just
                )
                (Json.Decode.oneOf
                    [ Json.Decode.at [ "req", "socket", "remoteAddress" ] Json.Decode.string |> Json.Decode.map Just
                    , Json.Decode.succeed Nothing
                    ]
                )
                (Json.Decode.at [ "req", "method" ] Json.Decode.string)
                (Json.Decode.at [ "req", "url" ] Json.Decode.string)
                (let
                    fn : List String -> Dict.Dict String (List String) -> Dict.Dict String (List String)
                    fn c acc =
                        case c of
                            first :: second :: rest ->
                                fn rest (Dict.update (String.toLower first) (\x -> x |> Maybe.withDefault [] |> (::) second |> Just) acc)

                            _ ->
                                acc
                 in
                 Json.Decode.at [ "req", "rawHeaders" ] (Json.Decode.list Json.Decode.string)
                    |> Json.Decode.map (\x -> fn x Dict.empty)
                )
                (Json.Decode.map2
                    (\fields files ->
                        Dict.merge
                            (\k v acc -> Dict.insert k v acc)
                            (\k v v2 acc -> Dict.insert k (v ++ v2) acc)
                            (\k v acc -> Dict.insert k v acc)
                            fields
                            files
                            Dict.empty
                    )
                    (Json.Decode.field "fields"
                        (Json.Decode.dict
                            (Json.Decode.list (Json.Decode.string |> Json.Decode.map StringPart))
                        )
                    )
                    (Json.Decode.field "files"
                        (Json.Decode.dict
                            (Json.Decode.list (fileDecoder |> Json.Decode.map FilePart))
                        )
                    )
                )

        toEvent : Json.Decode.Value -> Msg
        toEvent b =
            b
                |> Json.Decode.decodeValue
                    (Json.Decode.field "$" Json.Decode.int
                        |> Json.Decode.andThen
                            (\tag ->
                                case tag of
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
                                        Json.Decode.map GotRequest
                                            (Json.Decode.field "a" requestDecoder)

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


{-| <https://nodejs.org/api/net.html#serverlistenoptions-callback>
-}
type alias Options =
    { port_ : Maybe Int
    , host : Maybe String
    , path : Maybe String
    , backlog : Maybe Int
    , exclusive : Maybe Bool
    , readableAll : Maybe Bool
    , writableAll : Maybe Bool
    , ipv6Only : Maybe Bool
    }


emptyOptions : Options
emptyOptions =
    Options Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


optionsCodec : Codec.Codec Options
optionsCodec =
    Codec.object Options
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
type alias Request =
    { resource : Maybe RequestResource
    , ip : Maybe String
    , method : String
    , url : String
    , headers : Dict.Dict String (List String)
    , parts : Dict.Dict String (List Part)
    }


requestCodec : Codec.Codec Request
requestCodec =
    Codec.object Request
        |> Codec.field "resource" .resource (Codec.succeed Nothing)
        |> Codec.field "ip" .ip (Codec.maybe Codec.string)
        |> Codec.field "method" .method Codec.string
        |> Codec.field "url" .url Codec.string
        |> Codec.field "headers" .headers (Codec.dict (Codec.list Codec.string))
        |> Codec.field "parts" .parts (Codec.dict (Codec.list partCodec))
        |> Codec.buildObject



--


type RequestResource
    = RequestResource Json.Decode.Value Json.Decode.Value



--


type Part
    = StringPart String
    | FilePart File


partCodec : Codec.Codec Part
partCodec =
    Codec.custom
        (\fn1 fn2 x ->
            case x of
                StringPart x1 ->
                    fn1 x1

                FilePart x1 ->
                    fn2 x1
        )
        |> Codec.variant1 "StringPart" StringPart Codec.string
        |> Codec.variant1 "FilePart" FilePart fileCodec
        |> Codec.buildCustom



--


type alias File =
    { path : FileSystem.Path
    , name : String
    , mime : String
    , size : Int
    }


fileCodec : Codec.Codec File
fileCodec =
    Codec.object File
        |> Codec.field "path" .path (Codec.string |> Codec.map FileSystem.Path (\(FileSystem.Path x) -> x))
        |> Codec.field "name" .name Codec.string
        |> Codec.field "mime" .mime Codec.string
        |> Codec.field "size" .size Codec.int
        |> Codec.buildObject



--


type alias Response =
    { statusCode : Int
    , headers : Dict.Dict String (List String)
    , data : String
    }


responseCodec : Codec.Codec Response
responseCodec =
    Codec.object Response
        |> Codec.field "statusCode" .statusCode Codec.int
        |> Codec.field "headers" .headers (Codec.dict (Codec.list Codec.string))
        |> Codec.field "data" .data Codec.string
        |> Codec.buildObject


respond : Response -> Request -> Task.Task JavaScript.Error ()
respond response request =
    let
        files : List FileSystem.Path
        files =
            request.parts
                |> Dict.toList
                |> List.concatMap
                    (\( _, v ) ->
                        v
                            |> List.filterMap
                                (\v2 ->
                                    case v2 of
                                        StringPart _ ->
                                            Nothing

                                        FilePart v3 ->
                                            Just v3.path
                                )
                    )

        respond_ : Task.Task JavaScript.Error ()
        respond_ =
            case request.resource of
                Just b ->
                    JavaScript.run "new Promise((resolve, reject) => { a.res.writeHead(a.a, a.b); a.res.end(a.c, b => { b ? reject(b) : resolve() }) })"
                        (Json.Encode.object
                            [ ( "res", (\(RequestResource _ x) -> x) b )
                            , ( "a", response.statusCode |> Json.Encode.int )
                            , ( "b", response.headers |> Dict.toList |> List.concatMap (\( k, v ) -> v |> List.map (\v_ -> [ k, v_ ])) |> List.concat |> Json.Encode.list Json.Encode.string )
                            , ( "c", response.data |> Json.Encode.string )
                            ]
                        )
                        (Json.Decode.succeed ())

                Nothing ->
                    Task.succeed ()

        deleteFiles : Task.Task x ()
        deleteFiles =
            files
                |> List.map
                    (\v ->
                        v
                            |> FileSystem.delete
                            |> Task.onError (\_ -> Task.succeed ())
                    )
                |> Task.sequence
                |> Task.map (\_ -> ())
    in
    respond_
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.andThen
            (\v ->
                deleteFiles
                    |> Task.andThen
                        (\_ ->
                            case v of
                                Ok v2 ->
                                    Task.succeed v2

                                Err v2 ->
                                    Task.fail v2
                        )
            )
