port module Jsonp exposing (State, jsonpResponses, emptyState, view, wrapView, request)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Json.Decode
import String


port jsonpResponses : (Json.Decode.Value -> msg) -> Sub msg


type alias Script =
    { id : Int
    , url : String
    }


type alias PrivateState =
    { scripts : List Script
    , lastId : Int
    }


type State
    = State PrivateState


emptyState : State
emptyState =
    State (PrivateState [] 0)


viewKeyedScript : Script -> ( String, Html msg )
viewKeyedScript { id, url } =
    ( toString id, node "script" [ src url ] [] )


view : State -> Html msg
view (State { scripts }) =
    Html.Keyed.node "div"
        []
        (List.map viewKeyedScript scripts)


wrapView : (model -> State) -> (model -> Html msg) -> model -> Html msg
wrapView stateFn innerView model =
    let
        (State { scripts }) =
            stateFn model

        keyedChildren =
            ( "innerView", innerView model )
                :: (List.map viewKeyedScript scripts)
    in
        Html.Keyed.node "div" [] keyedChildren


appendQueryToUrl : String -> String -> String
appendQueryToUrl url query =
    let
        separator =
            if String.contains "?" url then
                "&"
            else
                "?"
    in
        url ++ separator ++ query


request : String -> State -> State
request url (State { scripts, lastId }) =
    let
        urlWithCallback =
            appendQueryToUrl url "callback=app.ports.jsonpResponses"

        id =
            lastId + 1

        newScript =
            { id = id, url = urlWithCallback }
    in
        State
            { scripts = [ newScript ]
            , lastId = id
            }
