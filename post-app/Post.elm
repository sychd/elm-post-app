module Post exposing (Post, PostId, idParser, idToString, postDecoder, postEncoder, postsDecoder)

import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



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


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <| \postId -> Maybe.map PostId (String.toInt postId)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id
