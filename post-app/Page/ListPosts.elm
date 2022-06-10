module Page.ListPosts exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Post exposing (Post, idToString, postsDecoder)
import RemoteData exposing (WebData)


type alias Model =
    { posts : WebData (List Post) }


type Msg
    = FetchPosts
    | PostsReceived (WebData (List Post))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading }, fetchPosts )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get { url = "http://localhost:5019/posts", expect = postsDecoder |> Http.expectJson (RemoteData.fromResult >> PostsReceived) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ refetchPostsButton model
        , viewPostsOrError model
        ]


refetchPostsButton : Model -> Html Msg
refetchPostsButton model =
    case model.posts of
        RemoteData.Loading ->
            text ""

        _ ->
            button [ onClick FetchPosts ] [ text "Refetch posts!" ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)

        RemoteData.Success posts ->
            viewPosts posts


viewError : String -> Html msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch :("
    in
    div []
        [ h3 []
            [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table [] (viewTableHeader :: List.map viewPost posts)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    tr []
        [ td []
            [ text (idToString post.id) ]
        , td []
            [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
