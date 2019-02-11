module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Parser exposing ((|.), (|=), Parser)



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
    , selectedPokemon : Maybe PokemonDetail
    }


type alias Pokemon =
    { id : Int
    , name : String
    }


type alias PokemonDetail =
    { id : Int
    , name : String
    , baseExperience : Int
    }


type PokemonType
    = Poison
    | Grass



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [ class "flex h-screen items-center" ]
                [ h1 [ class "text-5xl flex-1 text-center loading" ]
                    [ text "Loading"
                    , span [] [ text "." ]
                    , span [] [ text "." ]
                    , span [] [ text "." ]
                    ]
                ]

        Loaded { pokemonList } ->
            div [ class "flex" ]
                [ div [ class "w-1/5 bg-grey-darkest" ]
                    [ div [ class "fixed bg-grey-dark w-inherit z-10" ]
                        [ div [ class "px-2 py-4" ]
                            [ h1 [ class "text-2xl text-center text-white" ]
                                [ text "POKÉDEX" ]
                            ]
                        ]
                    , div [ class "max-h-screen overflow-y-auto pt-16" ] <|
                        List.map viewPokemonDetails pokemonList
                    ]
                , div [ class "w-4/5 flex items-center" ]
                    [ p [ class "text-5xl flex-1 text-center opacity-25" ]
                        [ text "CHOOSE A POKEMON" ]
                    ]
                ]

        Errored errorMsg ->
            div [ class "flex items-center h-screen" ]
                [ h1 [ class "text-5xl flex-1 text-center text-red" ] [ text errorMsg ]
                ]


viewPokemonDetails : Pokemon -> Html Msg
viewPokemonDetails pokemon =
    div [ class "flex mb-2 cursor-pointer" ]
        [ div [ class "w-1/5  mr-2 text-right text-grey-light opacity-50" ]
            [ text (String.fromInt pokemon.id) ]
        , div [ class "w-4/5 text-left text-white capitalize" ]
            [ text pokemon.name ]
        ]



-- UPDATE


type Msg
    = GotPokemonList (Result Http.Error (List Pokemon))
    | SelectedPokemon Int
    | GotPokemon (Result Http.Error PokemonDetail)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokemonList (Ok pokemonList) ->
            ( Loaded
                { pokemonList = pokemonList
                , selectedPokemon = Nothing
                }
            , Cmd.none
            )

        GotPokemonList (Err httpError) ->
            ( Errored "OOPS! Something failed"
            , Cmd.none
            )

        SelectedPokemon id ->
            ( model
            , getPokemon id
            )

        GotPokemon (Ok pokemon) ->
            ( updateLoadedModel (\internals -> { internals | selectedPokemon = Just pokemon })
                model
            , Cmd.none
            )

        GotPokemon (Err httpError) ->
            ( Errored "OOPS! Something failed while fetching a Pokemon"
            , Cmd.none
            )


{-| Helper function to update the model Loaded internals
-}
updateLoadedModel : (Internals -> Internals) -> Model -> Model
updateLoadedModel transform model =
    case model of
        Loaded internals ->
            Loaded (transform internals)

        _ ->
            model



-- HTTP


endpointUrl : String
endpointUrl =
    "https://pokeapi.co/api/v2"


pokemonUrl : String
pokemonUrl =
    endpointUrl ++ "/pokemon"


getPokemon : Int -> Cmd Msg
getPokemon id =
    Http.get
        { url = pokemonUrl ++ "/" ++ String.fromInt id
        , expect = Http.expectJson GotPokemon pokemonDetailDecoder
        }


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
        (Decode.field "url" urlIdDecoder)
        (Decode.field "name" Decode.string)


pokemonDetailDecoder : Decoder PokemonDetail
pokemonDetailDecoder =
    Decode.map3 PokemonDetail
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "base_experience" Decode.int)


urlIdDecoder : Decoder Int
urlIdDecoder =
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
