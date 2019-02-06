module Main exposing (main)

import Browser
import Html exposing (..)



-- MODEL


type Model
    = Loading
    | Loaded Internals
    | Errored String


init : ( Model, Cmd Msg )
init =
    ( Loading, Cmd.none )



-- TYPES


type alias Internals =
    { pokemonList : List Pokemon
    }


type alias Pokemon =
    { name : String
    }



-- VIEW


view : Model -> Html Msg
view model =
    text "Romario"



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
