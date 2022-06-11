module Page.ListPosts exposing (Model, Msg, init, update, view)

import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Post exposing (Post, PostId, idToString, postsDecoder)
import RemoteData exposing (WebData)


type alias Model =
    { posts : WebData (List Post), deleteError : Maybe String }


type Msg
    = FetchPosts
    | DeletePost PostId
    | PostsReceived (WebData (List Post))
    | PostDeleted (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading, deleteError = Nothing }, fetchPosts )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get { url = "http://localhost:5019/posts", expect = postsDecoder |> Http.expectJson (RemoteData.fromResult >> PostsReceived) }


deletePost : PostId -> Cmd Msg
deletePost postId =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect = Http.expectString PostDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        DeletePost postId ->
            ( model, deletePost postId )

        PostDeleted (Ok _) ->
            ( model, fetchPosts )

        PostDeleted (Err error) ->
            ( { model | deleteError = Just (buildErrorMessage error) }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ refetchPostsButton model
        , viewPostsOrError model
        , viewDeleteError model.deleteError
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


viewDeleteError : Maybe String -> Html msg
viewDeleteError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


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
    let
        postPath =
            "/posts/" ++ Post.idToString post.id
    in
    tr []
        [ td []
            [ text (idToString post.id) ]
        , td []
            [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
        , td []
            [ a [ href postPath ] [ text "Edit" ] ]
        , td []
            [ button [ type_ "button", onClick (DeletePost post.id) ]
                [ text "Delete" ]
            ]
        ]
