module Post exposing (Post, PostId, idToString, postDecoder, postsDecoder)

import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)



-- PostId is custom opaque type


type PostId
    = PostId Int


type alias Post =
    { id : PostId, title : String, authorName : String, authorUrl : String }


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


idDecoder : Decoder PostId
idDecoder =
    Json.Decode.map PostId int


postDecoder : Decoder Post
postDecoder =
    succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder
