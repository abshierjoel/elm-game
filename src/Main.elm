module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (all, any)
import List.Extra exposing (transpose)
import List.Split exposing (chunksOfLeft)
import Maybe exposing (withDefault)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { tiles =
        [ ( 0, Empty )
        , ( 1, Empty )
        , ( 2, Empty )
        , ( 3, Empty )
        , ( 4, Empty )
        , ( 5, Empty )
        , ( 6, Empty )
        , ( 7, Empty )
        , ( 8, Empty )
        ]
    , turn = Ex
    }



---- MODEL ----


type alias Model =
    { tiles : List ( Int, Tile )
    , turn : Tile
    }


type Tile
    = Ex
    | Oh
    | Empty


type Player
    = Exes
    | Ohs



---- UPDATE ----


type Msg
    = ClickedTile Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile pos ->
            ( makeMove model pos, Cmd.none )


isWinner : Tile -> List ( Int, Tile ) -> Bool
isWinner player spaces =
    all (\t -> t == True)
        [ spaces |> chunksOfLeft 3 |> any (allListMatches player)
        , spaces |> chunksOfLeft 3 |> transpose |> any (allListMatches player)
        ]


allListMatches : Tile -> List ( Int, Tile ) -> Bool
allListMatches player list =
    all (\item -> Tuple.second item == player) list


makeMove : Model -> Int -> Model
makeMove model pos =
    { model | turn = otherTurn model.turn, tiles = clickTile model.turn pos model.tiles }


clickTile : Tile -> Int -> List ( Int, Tile ) -> List ( Int, Tile )
clickTile player pos tiles =
    List.map
        (\( p, tile ) ->
            if p == pos then
                ( p, player )

            else
                ( p, tile )
        )
        tiles


otherTurn : Tile -> Tile
otherTurn player =
    case player of
        Ex ->
            Oh

        Oh ->
            Ex

        Empty ->
            Empty



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ viewBoard model.tiles
        ]


viewBoard : List ( Int, Tile ) -> Html Msg
viewBoard tiles =
    div [ class "ttt-board" ] (List.map viewTile tiles)


viewTile : ( Int, Tile ) -> Html Msg
viewTile ( pos, tile ) =
    case tile of
        Empty ->
            viewEmptyTile pos

        _ ->
            viewFilledTile tile


viewEmptyTile : Int -> Html Msg
viewEmptyTile pos =
    div [ class "tile", onClick <| ClickedTile pos ] []


viewFilledTile : Tile -> Html Msg
viewFilledTile tile =
    button [ class <| "tile " ++ getTileClass tile ] [ text <| getTileText tile ]


getTileClass : Tile -> String
getTileClass tile =
    case tile of
        Ex ->
            "ex"

        Oh ->
            "oh"

        Empty ->
            "empty"


getTileText : Tile -> String
getTileText tile =
    case tile of
        Ex ->
            "X"

        Oh ->
            "O"

        Empty ->
            ""
