port module Http.Server exposing
    ( Server, create, close
    , Options, emptyOptions, encodeOptions, optionsDecoder
    , Msg(..), onMsg
    , Request, RequestResource, Part(..), File, requestCodec, partCodec, fileCodec
    , Response, respond, responseCodec
    )

{-|

@docs Server, create, close

@docs Options, emptyOptions, encodeOptions, optionsDecoder

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
                b.on('error', e => { scope.ports.httpServer.send({ $: 0, a: e }) })
                b.on('request', (req, res) => {
                  req.on('error', e => { scope.ports.httpServer.send({ $: 1, a: e }) })
                  res.on('error', e => { scope.ports.httpServer.send({ $: 2, a: e }) })
                  res._created = Date.now()
                  c.default().parse(req, (e, fields, files) => {
                    scope.ports.httpServer.send(e ? { $: 1, a: e } : { $: 3, a: { req, res, fields, files } })
                  })
                })
                b.listen(a)
                return b
              })
            """
                (options |> encodeOptions)
                (Json.Decode.value |> Json.Decode.map Server)

        clearSocket : Task.Task x ()
        clearSocket =
            case options.path of
                Just b ->
                    FileSystem.delete (FileSystem.stringToPath b)
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


port httpServer : (Json.Decode.Value -> msg) -> Sub msg


type Msg
    = ServerError JavaScript.Error
    | RequestError JavaScript.Error
    | ResponseError JavaScript.Error
    | RequestReceived Request


onMsg : (Msg -> msg) -> Sub msg
onMsg toMsg =
    let
        fileDecoder : Json.Decode.Decoder File
        fileDecoder =
            Json.Decode.map4 File
                (Json.Decode.field "filepath" (Json.Decode.string |> Json.Decode.map FileSystem.stringToPath))
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
                    |> Json.Decode.map (\x -> Dict.empty |> fn x |> Dict.map (always List.reverse))
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

        toMsg_ : Json.Decode.Value -> Msg
        toMsg_ b =
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
                                        Json.Decode.map RequestReceived
                                            (Json.Decode.field "a" requestDecoder)

                                    _ ->
                                        Json.Decode.fail "Cannot decode message."
                            )
                    )
                |> (\x ->
                        case x of
                            Ok x2 ->
                                x2

                            Err x2 ->
                                ServerError (JavaScript.DecodeError x2)
                   )
    in
    httpServer (toMsg_ >> toMsg)



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


encodeOptions : Options -> Json.Encode.Value
encodeOptions a =
    [ a.port_ |> Maybe.map (Json.Encode.int >> Tuple.pair "port")
    , a.host |> Maybe.map (Json.Encode.string >> Tuple.pair "host")
    , a.path |> Maybe.map (Json.Encode.string >> Tuple.pair "path")
    , a.backlog |> Maybe.map (Json.Encode.int >> Tuple.pair "backlog")
    , a.exclusive |> Maybe.map (Json.Encode.bool >> Tuple.pair "exclusive")
    , a.readableAll |> Maybe.map (Json.Encode.bool >> Tuple.pair "readableAll")
    , a.writableAll |> Maybe.map (Json.Encode.bool >> Tuple.pair "writableAll")
    , a.ipv6Only |> Maybe.map (Json.Encode.bool >> Tuple.pair "ipv6Only")
    ]
        |> List.filterMap identity
        |> Json.Encode.object


optionsDecoder : Json.Decode.Decoder Options
optionsDecoder =
    let
        maybeField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
        maybeField name decoder =
            Json.Decode.oneOf
                [ Json.Decode.field name Json.Decode.value |> Json.Decode.map Just
                , Json.Decode.succeed Nothing
                ]
                |> Json.Decode.andThen
                    (\x ->
                        case x of
                            Just _ ->
                                Json.Decode.field name decoder
                                    |> Json.Decode.map Just

                            Nothing ->
                                Json.Decode.succeed Nothing
                    )
    in
    Json.Decode.map8 Options
        (maybeField "port" Json.Decode.int)
        (maybeField "host" Json.Decode.string)
        (maybeField "path" Json.Decode.string)
        (maybeField "backlog" Json.Decode.int)
        (maybeField "exclusive" Json.Decode.bool)
        (maybeField "readableAll" Json.Decode.bool)
        (maybeField "writableAll" Json.Decode.bool)
        (maybeField "ipv6Only" Json.Decode.bool)



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
    Codec.record Request
        |> Codec.field .resource (Codec.succeed Nothing)
        |> Codec.field .ip (Codec.maybe Codec.string)
        |> Codec.field .method Codec.string
        |> Codec.field .url Codec.string
        |> Codec.field .headers (Codec.dict Codec.string (Codec.list Codec.string))
        |> Codec.field .parts (Codec.dict Codec.string (Codec.list partCodec))
        |> Codec.buildRecord



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
        |> Codec.variant1 StringPart Codec.string
        |> Codec.variant1 FilePart fileCodec
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
    Codec.record File
        |> Codec.field .path (Codec.string |> Codec.map FileSystem.pathToString FileSystem.stringToPath)
        |> Codec.field .name Codec.string
        |> Codec.field .mime Codec.string
        |> Codec.field .size Codec.int
        |> Codec.buildRecord



--


type alias Response =
    { statusCode : Int
    , headers : Dict.Dict String (List String)
    , data : String
    }


responseCodec : Codec.Codec Response
responseCodec =
    Codec.record Response
        |> Codec.field .statusCode Codec.int
        |> Codec.field .headers (Codec.dict Codec.string (Codec.list Codec.string))
        |> Codec.field .data Codec.string
        |> Codec.buildRecord


respond : Response -> Request -> Task.Task JavaScript.Error ()
respond response request =
    let
        files : List FileSystem.Path
        files =
            request.parts
                |> Dict.toList
                |> List.concatMap
                    (\( _, x ) ->
                        x
                            |> List.filterMap
                                (\x2 ->
                                    case x2 of
                                        StringPart _ ->
                                            Nothing

                                        FilePart x3 ->
                                            Just x3.path
                                )
                    )

        respond_ : Task.Task JavaScript.Error ()
        respond_ =
            case request.resource of
                Just b ->
                    JavaScript.run "new Promise((resolve, reject) => { a.b.push('Server-Timing', 'Elm;dur=' + (Date.now() - a.res._created)); a.res.writeHead(a.a, a.b); a.res.end(a.c, b => { b ? reject(b) : resolve() }) })"
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
                    (\x ->
                        x
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
            (\x ->
                deleteFiles
                    |> Task.andThen
                        (\_ ->
                            case x of
                                Ok x2 ->
                                    Task.succeed x2

                                Err x2 ->
                                    Task.fail x2
                        )
            )
