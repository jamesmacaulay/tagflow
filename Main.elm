port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import String
import Regex exposing (Regex, HowMany(All, AtMost), regex)
import Http
import Json.Decode as Json exposing (Decoder, (:=))
import Jsonp
import Task exposing (Task)
import Time exposing (Time)
import Array exposing (Array)


config =
    { clientId = "06f0e80490044ad993f6cb6fd79e1149"
    , redirectUri = "http://localhost:8000/"
    }


main =
    Navigation.program urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = Jsonp.wrapView .jsonpState view
        , subscriptions = subscriptions
        }



-- JSON


type Media
    = Image String
    | Video String


imageDecoder : Decoder Media
imageDecoder =
    Json.at [ "images", "standard_resolution", "url" ] (Json.map Image Json.string)


videoDecoder : Decoder Media
videoDecoder =
    Json.at [ "videos", "standard_resolution", "url" ] (Json.map Video Json.string)


mediaDecoder : Decoder Media
mediaDecoder =
    Json.oneOf [ videoDecoder, imageDecoder ]


responseDecoder : Decoder (Array Media)
responseDecoder =
    "data" := Json.array mediaDecoder


recentTaggedMediaUrl : String -> String -> String
recentTaggedMediaUrl accessToken tag =
    "https://api.instagram.com/v1/tags/"
        ++ tag
        ++ "/media/recent?access_token="
        ++ accessToken



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
        |> (flip Maybe.andThen) (.submatches >> List.head >> Maybe.withDefault Nothing)


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


urlUpdate : Route -> Model -> ( Model, Cmd Msg )
urlUpdate route model =
    case route of
        HomeRoute ->
            ( { model | slideshow = Nothing }
            , Cmd.none
            )

        TagRoute tag ->
            case model.accessToken of
                Nothing ->
                    ( model, Navigation.newUrl "#" )

                Just accessToken ->
                    ( { model
                        | slideshow = Just (slideshow tag)
                        , tagInput = ""
                        , jsonpState = Jsonp.request (recentTaggedMediaUrl accessToken tag) model.jsonpState
                      }
                    , Cmd.none
                    )

        AccessTokenRoute token ->
            ( { model | accessToken = Just token }
            , Navigation.newUrl "#"
            )



-- MODEL


type alias Slideshow =
    { tag : String
    , media : Array Media
    , mediaIndex : Int
    }


type alias Model =
    { slideshow : Maybe Slideshow
    , tagInput : String
    , accessToken : Maybe String
    , jsonpState : Jsonp.State
    }


slideshow : String -> Slideshow
slideshow tag =
    Slideshow tag Array.empty 0


stepSlideshow : Slideshow -> Slideshow
stepSlideshow ({ media, mediaIndex } as slideshow) =
    let
        incremented =
            mediaIndex + 1

        loopedAround =
            if incremented >= Array.length media then
                0
            else
                incremented
    in
        { slideshow
            | mediaIndex = loopedAround
        }


empty : Model
empty =
    { slideshow = Nothing
    , tagInput = ""
    , accessToken = Nothing
    , jsonpState = Jsonp.emptyState
    }


init : Route -> ( Model, Cmd Msg )
init route =
    urlUpdate route empty



-- UPDATE


type Msg
    = TagInput String
    | Submit
    | ReceiveResponse Json.Value
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
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

        ReceiveResponse jsonValue ->
            case Json.decodeValue responseDecoder jsonValue of
                Err e ->
                    ( empty, Navigation.newUrl "#" )

                Ok mediaList ->
                    ( { model
                        | slideshow =
                            Maybe.map
                                (\s ->
                                    { s | media = mediaList }
                                )
                                model.slideshow
                      }
                    , Cmd.none
                    )

        Tick _ ->
            case model.slideshow of
                Nothing ->
                    ( model, Cmd.none )

                Just slideshow ->
                    ( { model
                        | slideshow = Just (stepSlideshow slideshow)
                      }
                    , Cmd.none
                    )



-- VIEW


authUrl : String -> String -> String
authUrl clientId redirectUri =
    "https://api.instagram.com/oauth/authorize/"
        ++ "?client_id="
        ++ clientId
        ++ "&redirect_uri="
        ++ redirectUri
        ++ "&response_type=token"
        ++ "&scope=public_content"


loginView : Html Msg
loginView =
    div []
        [ a [ href (authUrl config.clientId config.redirectUri) ]
            [ text "log in" ]
        ]


tagInputView : Model -> Html Msg
tagInputView { tagInput } =
    Html.form [ onSubmit Submit ]
        [ input [ onInput TagInput ] [ text tagInput ] ]


absoluteCenter : List ( String, String )
absoluteCenter =
    [ ( "position", "absolute" )
    , ( "top", "0" )
    , ( "bottom", "0" )
    , ( "left", "0" )
    , ( "right", "0" )
    , ( "margin", "auto" )
    ]


view : Model -> Html Msg
view model =
    case model.accessToken of
        Nothing ->
            loginView

        Just token ->
            case model.slideshow of
                Nothing ->
                    tagInputView model

                Just { media, mediaIndex } ->
                    case Array.get mediaIndex media of
                        Nothing ->
                            text "fetching..."

                        Just (Image url) ->
                            div []
                                [ img
                                    [ src url
                                    , style
                                        (absoluteCenter
                                            ++ [ ( "max-width", "100%" )
                                               , ( "max-height", "100%" )
                                               , ( "overflow", "auto" )
                                               ]
                                        )
                                    ]
                                    []
                                ]

                        Just (Video url) ->
                            text "[video]"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Jsonp.jsonpResponses ReceiveResponse
        , Time.every 5000 Tick
        ]
