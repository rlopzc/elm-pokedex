module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import JsonTree
import Parser exposing ((|.), (|=), Parser)
import RemoteData exposing (RemoteData)



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
    , selectedPokemon : RemoteData String PokemonDetail
    , jsonTreeState : JsonTree.State
    }


type alias Pokemon =
    { id : Int
    , name : String
    }


type alias PokemonDetail =
    { id : Int
    , name : String
    , baseExperience : Int
    , raw : Value
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
                [ viewLoadingText
                ]

        Loaded { pokemonList, selectedPokemon, jsonTreeState } ->
            div [ class "flex" ]
                [ div [ class "w-1/5 bg-grey-darkest" ]
                    [ div [ class "fixed bg-grey-dark w-inherit z-10" ]
                        [ div [ class "px-2 py-4" ]
                            [ h1 [ class "text-2xl text-center text-white" ]
                                [ text "POKÉDEX" ]
                            ]
                        ]
                    , div [ class "max-h-screen overflow-y-auto pt-16" ] <|
                        List.map viewPokemonIdName pokemonList
                    ]
                , div [ class "w-4/5 flex items-center" ]
                    [ case selectedPokemon of
                        RemoteData.Success pokemon ->
                            viewPokemonDetail pokemon jsonTreeState

                        RemoteData.Loading ->
                            viewLoadingText

                        RemoteData.NotAsked ->
                            p [ class "text-5xl flex-1 text-center opacity-25" ]
                                [ text "CHOOSE A POKEMON" ]

                        RemoteData.Failure errorMsg ->
                            p [ class "text-5xl flex-1 text-center opacity-25" ]
                                [ text errorMsg ]
                    ]
                ]

        Errored errorMsg ->
            div [ class "flex items-center h-screen" ]
                [ h1 [ class "text-5xl flex-1 text-center text-red" ] [ text errorMsg ]
                ]


viewLoadingText : Html Msg
viewLoadingText =
    h1 [ class "text-5xl flex-1 text-center loading" ]
        [ text "Loading"
        , span [] [ text "." ]
        , span [] [ text "." ]
        , span [] [ text "." ]
        ]


viewPokemonIdName : Pokemon -> Html Msg
viewPokemonIdName pokemon =
    div [ class "flex mb-2 cursor-pointer", onClick (SelectedPokemon pokemon.id) ]
        [ div [ class "w-1/5  mr-2 text-right text-grey-light opacity-50" ]
            [ text (String.fromInt pokemon.id) ]
        , div [ class "w-4/5 text-left text-white capitalize" ]
            [ text pokemon.name ]
        ]


viewPokemonDetail : PokemonDetail -> JsonTree.State -> Html Msg
viewPokemonDetail pokemon jsonTreeState =
    div [ class "flex flex-col h-screen w-full" ]
        [ div [ class "flex-1 mx-auto flex justify-center flex-col" ]
            [ viewSprite pokemon.id
            , p [ class "pt-1 text-center" ]
                [ div [ class "uppercase text-2xl" ]
                    [ text pokemon.name ]
                , text ("No. " ++ padLeft pokemon.id)
                ]
            ]
        , div [ class "flex-1 flex" ]
            [ div [ class "w-1/2" ]
                [ p [ class "text-center text-lg pb-2" ]
                    [ text "INFORMATION" ]
                , table [ class "mx-auto w-full" ]
                    [ tbody []
                        [ tr []
                            [ th [] [ text "Base Exp" ]
                            , td [] [ text (String.fromInt pokemon.baseExperience) ]
                            ]
                        ]
                    ]
                ]
            , div [ class "w-1/2" ]
                [ p [ class "text-center text-lg pb-2" ]
                    [ text "RAW" ]
                , div [ class "mx-auto w-full overflow-y-auto", style "height" "21rem" ]
                    [ JsonTree.parseValue pokemon.raw
                        |> Result.map (\tree -> JsonTree.view tree jsonTreeConfig jsonTreeState)
                        |> Result.withDefault (text "Failed to parse JSON")
                    ]
                ]
            ]
        ]


jsonTreeConfig =
    { onSelect = Nothing, toMsg = GotJsonTreeMsg }


viewSprite : Int -> Html msg
viewSprite id =
    div [ class "bg-grey-lighter p-3 text-center" ]
        [ img [ src (imgUrl id), class "w-1/2" ] []
        ]


{-| This URL provide full images of the pokemon. The pokemon number is specidied like: 001, ...,
999
-}
imgUrl : Int -> String
imgUrl id =
    "https://assets.pokemon.com/assets/cms2/img/pokedex/full/"
        ++ padLeft id
        ++ ".png"


padLeft : Int -> String
padLeft id =
    String.fromInt id
        |> String.padLeft 3 '0'



-- UPDATE


type Msg
    = GotPokemonList (Result Http.Error (List Pokemon))
    | SelectedPokemon Int
    | GotPokemon (Result Http.Error PokemonDetail)
    | GotJsonTreeMsg JsonTree.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokemonList (Ok pokemonList) ->
            ( Loaded
                { pokemonList = pokemonList
                , selectedPokemon = RemoteData.NotAsked
                , jsonTreeState = JsonTree.defaultState
                }
            , Cmd.none
            )

        GotPokemonList (Err httpError) ->
            ( Errored "OOPS! Something failed"
            , Cmd.none
            )

        SelectedPokemon id ->
            ( updateLoadedModel (\internals -> { internals | selectedPokemon = RemoteData.Loading }) model
            , getPokemon id
            )

        GotPokemon (Ok pokemon) ->
            let
                initialTreeState =
                    case JsonTree.parseValue pokemon.raw of
                        Ok rootNode ->
                            JsonTree.collapseToDepth 1 rootNode JsonTree.defaultState

                        Err _ ->
                            JsonTree.defaultState
            in
            ( updateLoadedModel
                (\internals ->
                    { internals
                        | selectedPokemon = RemoteData.Success pokemon
                        , jsonTreeState = initialTreeState
                    }
                )
                model
            , Cmd.none
            )

        GotPokemon (Err httpError) ->
            ( updateLoadedModel
                (\internals ->
                    { internals
                        | selectedPokemon = RemoteData.Failure "OOPS! Something failed while fetching a Pokemon"
                    }
                )
                model
            , Cmd.none
            )

        GotJsonTreeMsg jsonTreeNewState ->
            ( updateLoadedModel (\internals -> { internals | jsonTreeState = jsonTreeNewState }) model
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
    Decode.map4 PokemonDetail
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "base_experience" Decode.int)
        Decode.value


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
