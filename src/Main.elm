module Main exposing (main)

import Browser
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Parser exposing ((|.), (|=), Parser)
import String.Extra as String



-- MODEL


type Model
    = Loading
    | Loaded Internals
    | Errored String


init : ( Model, Cmd Msg )
init =
    ( Loading, getPokemonList )



-- TYPES


type alias Internals =
    { pokemonList : List Pokemon
    }


type alias Pokemon =
    { id : Int
    , name : String
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Spinner"

        Loaded { pokemonList } ->
            div [] <|
                List.map viewPokemonDetails pokemonList

        Errored errorMsg ->
            text errorMsg


viewPokemonDetails : Pokemon -> Html Msg
viewPokemonDetails pokemon =
    p []
        [ text (String.fromInt pokemon.id)
        , text " - "
        , text pokemon.name
        ]



-- UPDATE


type Msg
    = GotPokemonList (Result Http.Error (List Pokemon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokemonList (Ok pokemonList) ->
            ( Loaded { pokemonList = pokemonList }
            , Cmd.none
            )

        GotPokemonList (Err response) ->
            ( Errored "Something failed, contact the Administrator"
            , Cmd.none
            )



-- HTTP


endpointUrl : String
endpointUrl =
    "https://pokeapi.co/api/v2"


pokemonUrl : String
pokemonUrl =
    endpointUrl ++ "/pokemon"


getPokemonList : Cmd Msg
getPokemonList =
    Http.get
        { url = pokemonUrl ++ "?limit=151"
        , expect = Http.expectJson GotPokemonList pokemonListDecoder
        }



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- SERIALIZATION


pokemonListDecoder : Decoder (List Pokemon)
pokemonListDecoder =
    Decode.field "results"
        (Decode.list pokemonDecoder)


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Decode.map2 Pokemon
        (Decode.field "url" idDecoder)
        (Decode.map String.toSentenceCase (Decode.field "name" Decode.string))


idDecoder : Decoder Int
idDecoder =
    Decode.string
        |> Decode.andThen (fromResult << Parser.run idParser)


{-| The ID is in the URL sent from the backend

        url : String
        url =
            "https://pokeapi.co/api/v2/pokemon/1/"

-}
idParser : Parser Int
idParser =
    Parser.succeed identity
        |. Parser.token pokemonUrl
        |. Parser.symbol "/"
        |= Parser.int
        |. Parser.symbol "/"
        |. Parser.end


fromResult : Result b a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            Decode.succeed a

        Err _ ->
            Decode.fail "Failed to decode"
