module Page.EditPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page.ListPosts exposing (Msg)
import Post exposing (Post, PostId, postDecoder, postEncoder)
import RemoteData
import Route


type alias Model =
    { navKey : Nav.Key
    , post : RemoteData.WebData Post
    , saveError : Maybe String
    }


type Msg
    = PostReceived (RemoteData.WebData Post)
    | UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | SavePost
    | PostSaved (Result Http.Error Post)


type Field
    = Title
    | Name
    | Url


init : PostId -> Nav.Key -> ( Model, Cmd Msg )
init postId navKey =
    ( initialModel navKey, fetchPost postId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , post = RemoteData.Loading
    , saveError = Nothing
    }


fetchPost : PostId -> Cmd Msg
fetchPost postId =
    Http.get
        { url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect =
            postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

        UpdateTitle value ->
            ( { model | post = updateField model.post Title value }, Cmd.none )

        UpdateAuthorName value ->
            ( { model | post = updateField model.post Name value }, Cmd.none )

        UpdateAuthorUrl value ->
            ( { model | post = updateField model.post Url value }, Cmd.none )

        SavePost ->
            ( model, savePost model.post )

        PostSaved (Ok postData) ->
            ( { model | post = RemoteData.succeed postData, saveError = Nothing }
            , Route.pushUrl Route.Posts model.navKey
            )

        PostSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }, Cmd.none )


updateField : RemoteData.WebData Post -> Field -> String -> RemoteData.WebData Post
updateField postData field value =
    let
        updFn : (Post -> Post) -> RemoteData.WebData Post
        updFn fn =
            RemoteData.map
                fn
                postData
    in
    case field of
        Name ->
            updFn (\post -> { post | authorName = value })

        Title ->
            updFn (\post -> { post | title = value })

        Url ->
            updFn (\post -> { post | authorUrl = value })


savePost : RemoteData.WebData Post -> Cmd Msg
savePost post =
    case post of
        RemoteData.Success postData ->
            let
                postUrl =
                    "http://localhost:5019/posts/" ++ Post.idToString postData.id
            in
            Http.request
                { method = "PATCH"
                , url = postUrl
                , body = Http.jsonBody (postEncoder postData)
                , expect = Http.expectJson PostSaved postDecoder
                , tracker = Nothing
                , timeout = Nothing
                , headers = []
                }

        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit post" ]
        , viewPost model.post
        , viewSaveError model.saveError
        ]


viewPost : RemoteData.WebData Post -> Html Msg
viewPost post =
    case post of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Post..." ]

        RemoteData.Success postData ->
            editForm postData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : Post -> Html Msg
editForm post =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value post.title
                , onInput UpdateTitle
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Author Name"
            , br [] []
            , input
                [ type_ "text"
                , value post.authorName
                , onInput UpdateAuthorName
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Author URL"
            , br [] []
            , input
                [ type_ "text"
                , value post.authorUrl
                , onInput UpdateAuthorUrl
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick SavePost ]
                [ text "Submit" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch post at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
