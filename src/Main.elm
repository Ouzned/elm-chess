module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List
import List.Extra as List
import Maybe.Extra as Maybe



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Color
    = White
    | Black


type PieceType
    = Pawn Bool
    | King
        { kingCastling : Bool
        , queenCastling : Bool
        }
    | Queen
    | Rook
    | Knight
    | Bishop


type alias Piece =
    { pieceType : PieceType
    , color : Color
    }


type alias Row =
    Int


type alias Col =
    Int


type alias Cell =
    ( Row, Col )


type alias Board =
    Dict.Dict Cell Piece


type Game
    = Game
        { player : Color
        , board : Board
        , gameHistory : List Game
        , moveHistory : List Move
        }


type alias Move =
    { src : Cell
    , dst : Cell
    , moveType : MoveType
    }


type MoveType
    = Standard
    | PawnStart
    | EnPassant Cell
    | Castling
        { rook : Cell
        , rookDst : Cell
        }


type CastlingType
    = CastlingQueen
    | CastlingKing


type Direction
    = Up
    | Down
    | Left
    | Right
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight


newGame : Game
newGame =
    Game
        { player = White
        , board = startingBoard
        , gameHistory = []
        , moveHistory = []
        }


startingBoard : Board
startingBoard =
    let
        row index =
            List.map (Tuple.pair index) (List.range 1 8)

        pawns =
            List.repeat 8
                (Piece <|
                    Pawn False
                )

        majorPieces =
            List.map Piece
                [ Rook
                , Knight
                , Bishop
                , King { kingCastling = True, queenCastling = True }
                , Queen
                , Bishop
                , Knight
                , Rook
                ]

        rowPieces index color pieces =
            List.map2 Tuple.pair (row index) (List.map (\p -> p color) pieces)

        allPieces =
            rowPieces 1 White majorPieces
                ++ rowPieces 2 White pawns
                ++ rowPieces 7 Black pawns
                ++ rowPieces 8 Black majorPieces

        insertPiece ( cell, piece ) board =
            Dict.insert cell piece board
    in
    List.foldl insertPiece Dict.empty allPieces


move_ : Cell -> Cell -> Board -> Board
move_ src dst board =
    let
        insert cell piece =
            Dict.insert cell piece

        remove cell =
            Dict.remove cell
    in
    case Dict.get src board of
        Nothing ->
            board

        Just piece ->
            (remove src << insert dst piece) board


pieceAt : Board -> Cell -> Maybe Piece
pieceAt board cell =
    Dict.get cell board


isEmpty : Board -> Cell -> Bool
isEmpty board cell =
    pieceAt board cell == Nothing


hasPiece : Board -> Cell -> Bool
hasPiece board =
    not << isEmpty board


isColor : Color -> Board -> Cell -> Bool
isColor color board cell =
    let
        sameColor =
            (==) color << .color
    in
    pieceAt board cell
        |> Maybe.unwrap False sameColor


opposite : Color -> Color
opposite color =
    if color == White then
        Black

    else
        White


availableCells : Board -> Cell -> Move -> List Move
availableCells board cell prevMove =
    case pieceAt board cell of
        Nothing ->
            []

        Just { pieceType, color } ->
            case pieceType of
                Queen ->
                    queenMoves board cell color

                Rook ->
                    rookMoves board cell color

                Bishop ->
                    bishopMoves board cell color

                Pawn hasMoved ->
                    pawnMoves board cell color hasMoved
                        ++ Maybe.values [ enPassant cell color prevMove ]

                King att ->
                    []

                Knight ->
                    knightMoves board cell color



-- CELL FUNCTIONS --


fromInt : { a | row : Int, col : Int } -> Maybe Cell
fromInt { row, col } =
    let
        isValid a =
            a > 0 && a < 9
    in
    if isValid row && isValid col then
        Just ( row, col )

    else
        Nothing


isNextTo : Cell -> Cell -> Bool
isNextTo ( row, col ) ( row_, col_ ) =
    row == row_ && abs (col - col_) == 1


kingOrigin : Color -> Cell
kingOrigin color =
    if color == White then
        ( 1, 4 )

    else
        ( 8, 4 )


leftRookOrigin : Color -> Cell
leftRookOrigin color =
    if color == White then
        ( 1, 1 )

    else
        ( 8, 1 )


cellInDirection : Direction -> Cell -> Maybe Cell
cellInDirection dir start =
    let
        ( row, col ) =
            start
    in
    case dir of
        Up ->
            fromInt { row = row + 1, col = col }

        Down ->
            fromInt { row = row - 1, col = col }

        Left ->
            fromInt { row = row, col = col - 1 }

        Right ->
            fromInt { row = row, col = col + 1 }

        UpLeft ->
            cellInDirections start [ Up, Left ]

        UpRight ->
            cellInDirections start [ Up, Right ]

        DownLeft ->
            cellInDirections start [ Down, Left ]

        DownRight ->
            cellInDirections start [ Down, Right ]


cellInDirections : Cell -> List Direction -> Maybe Cell
cellInDirections start dirs =
    let
        nextDir dir cell =
            Maybe.andThen (cellInDirection dir) cell
    in
    List.foldl nextDir (Just start) dirs


step : Board -> Direction -> Cell -> Maybe Cell
step board dir src =
    cellInDirection dir src
        |> Maybe.filter (isEmpty board)


stepAll : Board -> Direction -> Cell -> Maybe Cell
stepAll board dir start =
    step board dir start
        |> Maybe.andThen (stepAll board dir)


repeat : (Cell -> Maybe Cell) -> Cell -> List Cell
repeat action start =
    let
        doRepeat c =
            c :: repeat action c
    in
    action start
        |> Maybe.unwrap [] doRepeat


capture : Board -> Cell -> Color -> Direction -> Maybe Cell
capture board src color dir =
    let
        isOpponent =
            isColor (opposite color) board

        targetCell =
            cellInDirection dir src
    in
    Maybe.filter isOpponent targetCell


moveOrCapture : Board -> Cell -> Color -> Direction -> Maybe Cell
moveOrCapture board start color dir =
    Maybe.or (step board dir start) (capture board start color dir)



-- MOVE FUNCTIONS --


toStandardMove : Cell -> Cell -> Move
toStandardMove src dst =
    { src = src, dst = dst, moveType = Standard }


moveOrCaptureDirections : Board -> Cell -> Color -> List Direction -> List Move
moveOrCaptureDirections board start color dirs =
    let
        doMove dir cell =
            moveOrCapture board cell color dir

        moveDir dir =
            repeat (doMove dir) start
    in
    dirs
        |> List.concatMap moveDir
        |> List.map (toStandardMove start)


knightMoves : Board -> Cell -> Color -> List Move
knightMoves board src color =
    let
        dirs =
            [ [ Up, Up, Left ]
            , [ Up, Up, Right ]
            , [ Up, Right, Right ]
            , [ Up, Left, Left ]
            , [ Down, Down, Right ]
            , [ Down, Down, Left ]
            , [ Down, Left, Left ]
            , [ Down, Right, Right ]
            ]

        jump =
            cellInDirections src

        otherColorOrEmpty =
            not << isColor color board
    in
    List.filterMap jump dirs
        |> List.filter otherColorOrEmpty
        |> List.map (toStandardMove src)


bishopMoves : Board -> Cell -> Color -> List Move
bishopMoves board start color =
    [ UpRight, UpLeft, DownRight, DownLeft ]
        |> moveOrCaptureDirections board start color


queenMoves : Board -> Cell -> Color -> List Move
queenMoves board start color =
    [ Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight ]
        |> moveOrCaptureDirections board start color


rookMoves : Board -> Cell -> Color -> List Move
rookMoves board start color =
    [ Up, Down, Left, Right ]
        |> moveOrCaptureDirections board start color


pawnMoves : Board -> Cell -> Color -> Bool -> List Move
pawnMoves board src color hasMoved =
    let
        stepDir =
            pawnForward color

        captureDirs =
            pawnCaptures color

        steps =
            repeat (step board stepDir) src
                |> List.take (pawnMoveNb hasMoved)

        captures =
            List.filterMap (capture board src color) captureDirs
    in
    List.map (toStandardMove src) (steps ++ captures)


enPassant : Cell -> Color -> Move -> Maybe Move
enPassant start color prevMove =
    let
        opponentCell =
            prevMove.dst

        targetCell =
            cellInDirection (pawnForward color) opponentCell

        toMove dst =
            Just <|
                { src = start
                , dst = dst
                , moveType =
                    EnPassant opponentCell
                }
    in
    case prevMove.moveType of
        PawnStart ->
            if isNextTo start opponentCell then
                Maybe.andThen toMove targetCell

            else
                Nothing

        _ ->
            Nothing


castling : Board -> Cell -> Color -> Direction -> Maybe Move
castling board start color dir =
    let
        targetCell =
            cellInDirections start
                [ dir, dir ]
    in
    Nothing


pawnMoveNb : Bool -> Int
pawnMoveNb hasMoved =
    if hasMoved then
        1

    else
        2


pawnCaptures : Color -> List Direction
pawnCaptures color =
    if color == White then
        [ UpRight, UpLeft ]

    else
        [ DownRight, DownLeft ]


pawnForward : Color -> Direction
pawnForward color =
    if color == White then
        Up

    else
        Down



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
