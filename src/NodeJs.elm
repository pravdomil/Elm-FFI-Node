module NodeJs exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task


fixHttp : Task.Task JavaScript.Error ()
fixHttp =
    JavaScript.run "(() => { global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest; Object.defineProperty(XMLHttpRequest.prototype, 'response', { get() { return this.responseText } }) })()"
        Json.Encode.null
        (Json.Decode.succeed ())
