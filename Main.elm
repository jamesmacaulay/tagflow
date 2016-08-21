module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import String
import Regex exposing (Regex, HowMany(All, AtMost), regex)


config =
    { clientId = "06f0e80490044ad993f6cb6fd79e1149"
    , redirectUri = "http://localhost:8000/Main.elm"
    }


main =
    Navigation.program
        urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }


-- URLs


type Route
    = HomeRoute
    | TagRoute String
    | AccessTokenRoute String


accessTokenFromHash : String -> Maybe String
accessTokenFromHash hash =
    hash
        |> Regex.find All (regex "^#access_token=(.*)")
        |> List.head
        |> (flip Maybe.andThen)
            (.submatches >> List.head >> Maybe.withDefault Nothing)


tagFromHash : String -> String
tagFromHash =
    Regex.replace (AtMost 1) (regex "#") (always "")


routeFromHash : String -> Route
routeFromHash hash =
    if String.isEmpty hash then
        HomeRoute
    else
        case accessTokenFromHash hash of
            Just token ->
                AccessTokenRoute token

            Nothing ->
                TagRoute (tagFromHash hash)


urlParser : Navigation.Parser Route
urlParser =
    Navigation.makeParser (routeFromHash << .hash)


urlUpdate : Route -> Model -> (Model, Cmd Msg)
urlUpdate route model =
    case route of
        HomeRoute ->
            ( { model | tag = Nothing }
            , Cmd.none
            )

        TagRoute tag ->
            ( { model | tag = Just tag, tagInput = "" }
            , Cmd.none
            )

        AccessTokenRoute token ->
            ( { model | accessToken = Just token }
            , Cmd.none
            )


-- MODEL

type alias Model =
    { tag : Maybe String
    , tagInput : String
    , accessToken : Maybe String
    }


empty : Model
empty =
    Model Nothing "" Nothing


init : Route -> (Model, Cmd Msg)
init route =
    urlUpdate route empty


-- UPDATE


type Msg
    = TagInput String
    | Submit


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TagInput str ->
            ( { model | tagInput = str }
            , Cmd.none
            )

        Submit ->
            ( model
            , Navigation.newUrl ("#" ++ model.tagInput)
            )


-- VIEW


authUrl : String -> String -> String
authUrl clientId redirectUri =
    "https://api.instagram.com/oauth/authorize/?client_id=" ++ clientId ++
    "&redirect_uri=" ++ redirectUri ++
    "&response_type=token"


loginView : Html Msg
loginView =
    div
        []
        [ a
            [ href (authUrl config.clientId config.redirectUri) ]
            [ text "log in"]
        ]

tagInputView : Html Msg
tagInputView =
    Html.form
        [ onSubmit Submit ]
        [ input
            [ onInput TagInput ]
            [ ]
        ]

view : Model -> Html Msg
view ({tag, accessToken} as model) =
    case accessToken of
        Nothing ->
            loginView

        Just token ->
            case tag of
                Nothing ->
                    tagInputView

                Just tag ->
                    model |> toString |> text





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
