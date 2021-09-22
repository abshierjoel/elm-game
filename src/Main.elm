module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { tiles =
        [ ( 0, Maybe.Nothing )
        , ( 1, Maybe.Nothing )
        , ( 2, Maybe.Nothing )
        , ( 3, Maybe.Nothing )
        , ( 4, Maybe.Nothing )
        , ( 5, Maybe.Nothing )
        , ( 6, Maybe.Nothing )
        , ( 7, Maybe.Nothing )
        , ( 8, Maybe.Nothing )
        ]
    , turn = Ex
    }



---- MODEL ----


type alias Model =
    { tiles : List ( Int, Maybe Tile )
    , turn : Tile
    }


type Tile
    = Ex
    | Oh



---- UPDATE ----


type Msg
    = ClickedTile Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile pos ->
            ( makeMove model pos, Cmd.none )


makeMove : Model -> Int -> Model
makeMove model pos =
    { model | turn = otherTurn model.turn, tiles = clickTile model.turn pos model.tiles }


clickTile : Tile -> Int -> List ( Int, Maybe Tile ) -> List ( Int, Maybe Tile )
clickTile player pos tiles =
    List.map
        (\( p, tile ) ->
            if p == pos then
                ( p, Just player )

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



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ viewBoard model.tiles
        ]


viewBoard : List ( Int, Maybe Tile ) -> Html Msg
viewBoard tiles =
    div [ class "ttt-board" ] (List.map viewTile tiles)


viewTile : ( Int, Maybe Tile ) -> Html Msg
viewTile ( pos, tile ) =
    case tile of
        Just t ->
            viewFilledTile t

        Nothing ->
            viewEmptyTile pos


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


getTileText : Tile -> String
getTileText tile =
    case tile of
        Ex ->
            "X"

        Oh ->
            "O"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
