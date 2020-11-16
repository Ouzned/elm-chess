module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (dir, src)
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
    = Pawn { hasMoved : Bool }
    | King { kingCastling : Bool, queenCastling : Bool }
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
                (Piece <| Pawn { hasMoved = False })

        majorPieces =
            List.map Piece <|
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


oppositeColor : Color -> Color
oppositeColor color =
    if color == White then
        Black

    else
        White


availableCells : Board -> Cell -> Move -> List Move
availableCells board cell prevMove =
    let
        getMoves piece =
            case piece.pieceType of
                Queen ->
                    queenMoves board cell

                Rook ->
                    rookMoves board cell

                Bishop ->
                    bishopMoves board cell

                Pawn { hasMoved } ->
                    pawnMoves board cell piece.color hasMoved
                        ++ Maybe.values [ enPassant cell piece.color prevMove ]

                King att ->
                    []

                Knight ->
                    knightMoves board cell piece.color
    in
    pieceAt board cell
        |> Maybe.unwrap [] getMoves



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


cellInDirection : Cell -> Direction -> Maybe Cell
cellInDirection start dir =
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


allCellsInDirection : Cell -> Direction -> List Cell
allCellsInDirection start dir =
    let
        doRepeat c =
            c :: allCellsInDirection c dir
    in
    cellInDirection start dir
        |> Maybe.unwrap [] doRepeat


cellInDirections : Cell -> List Direction -> Maybe Cell
cellInDirections start dirs =
    let
        flipMove dir cell =
            cellInDirection cell dir

        nextDir dir cell =
            Maybe.andThen (flipMove dir) cell
    in
    List.foldl nextDir (Just start) dirs


captureDirection : Board -> Color -> Cell -> Direction -> Maybe Cell
captureDirection board color start dir =
    let
        isNotOpponent cell =
            isEmpty board cell || isColor color board cell
    in
    allCellsInDirection start dir
        |> List.dropWhile isNotOpponent
        |> List.head


capture : Board -> Color -> Cell -> Direction -> Maybe Cell
capture board color start dir =
    let
        isOpponent =
            isColor (oppositeColor color) board
    in
    cellInDirection start dir
        |> Maybe.filter isOpponent


step : Board -> Cell -> Direction -> Maybe Cell
step board start dir =
    cellInDirection start dir
        |> Maybe.filter (isEmpty board)


moveDirection : Board -> Cell -> Direction -> List Cell
moveDirection board start dir =
    step board start dir
        |> Maybe.map (\c -> c :: moveDirection board c dir)
        |> Maybe.withDefault []


moveAllDirections : Board -> Cell -> List Direction -> List Cell
moveAllDirections board start dirs =
    List.concatMap (moveDirection board start) dirs


captureAllDirections : Board -> Cell -> Color -> List Direction -> List Cell
captureAllDirections board start color dirs =
    List.filterMap (captureDirection board color start) dirs



-- PIECE MOVES --


toStandardMove : Cell -> Cell -> Move
toStandardMove src dst =
    { src = src, dst = dst, moveType = Standard }


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


pawnForward : Color -> Direction
pawnForward color =
    if color == White then
        Up

    else
        Down


rookDirections : List Direction
rookDirections =
    [ Up, Down, Left, Right ]


bishopDirections : List Direction
bishopDirections =
    [ UpRight, UpLeft, DownRight, DownLeft ]


kingDirections : List Direction
kingDirections =
    bishopDirections ++ rookDirections


queenDirections : List Direction
queenDirections =
    kingDirections


rookMoves : Board -> Cell -> List Move
rookMoves board start =
    moveAllDirections board start rookDirections
        |> List.map (toStandardMove start)


kingMoves : Board -> Cell -> List Move
kingMoves board start =
    kingDirections
        |> List.map (step board start)
        |> Maybe.values
        |> List.map (toStandardMove start)


kingCaptures : Board -> Cell -> Color -> List Move
kingCaptures board start color =
    kingDirections
        |> List.map (capture board color start)
        |> Maybe.values
        |> List.map (toStandardMove start)


bishopMoves : Board -> Cell -> List Move
bishopMoves board start =
    moveAllDirections board start bishopDirections
        |> List.map (toStandardMove start)


bishopCaptures : Board -> Cell -> Color -> List Move
bishopCaptures board start color =
    captureAllDirections board start color bishopDirections
        |> List.map (toStandardMove start)


queenMoves : Board -> Cell -> List Move
queenMoves board start =
    moveAllDirections board start queenDirections
        |> List.map (toStandardMove start)


queenCaptures : Board -> Cell -> Color -> List Move
queenCaptures board start color =
    captureAllDirections board start color queenDirections
        |> List.map (toStandardMove start)


pawnMoves : Board -> Cell -> Color -> Bool -> List Move
pawnMoves board start color hasMoved =
    let
        moveNb =
            if hasMoved then
                1

            else
                2

        pawnDir =
            pawnForward color

        firstStep =
            step board start pawnDir

        secondStep =
            firstStep
                |> Maybe.andThen
                    (\c ->
                        step board c pawnDir
                    )
    in
    [ firstStep, secondStep ]
        |> List.take moveNb
        |> Maybe.values
        |> List.map (toStandardMove start)


pawnCaptures : Board -> Cell -> Color -> List Move
pawnCaptures board start color =
    let
        captureDirections =
            if color == White then
                [ UpRight, UpLeft ]

            else
                [ DownRight, DownLeft ]
    in
    captureDirections
        |> List.map (capture board color start)
        |> Maybe.values
        |> List.map (toStandardMove start)


enPassant : Cell -> Color -> Move -> Maybe Move
enPassant start color prevMove =
    let
        opponentCell =
            prevMove.dst

        targetCell =
            cellInDirection opponentCell (pawnForward color)

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
