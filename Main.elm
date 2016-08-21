module Main exposing (..)

import Html exposing (..)
import Navigation
import String
import Regex exposing (Regex, HowMany(All, AtMost), regex)


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
            ( { model | tag = Just tag }
            , Cmd.none
            )

        AccessTokenRoute token ->
            ( { model | accessToken = Just token }
            , Cmd.none
            )


-- MODEL

type alias Model =
    { tag : Maybe String
    , accessToken : Maybe String
    }


empty : Model
empty =
    Model Nothing Nothing


init : Route -> (Model, Cmd Msg)
init route =
    urlUpdate route empty


-- UPDATE


type alias Msg = Never


update : Msg -> Model -> (Model, Cmd Msg)
update _ model =
    ( model , Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    model |> toString |> text


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
