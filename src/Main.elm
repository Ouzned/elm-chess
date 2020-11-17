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
    = Pawn Bool
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
        , lastMove : Maybe Move
        , isCheck : Bool
        , isCheckmate : Bool
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


type alias Direction =
    ( Row, Col )


newGame : Game
newGame =
    Game
        { player = White
        , board = startingBoard
        , gameHistory = []
        , lastMove = Nothing
        , isCheck = False
        , isCheckmate = False
        }


startingBoard : Board
startingBoard =
    let
        row index =
            List.map (Tuple.pair index) (List.range 1 8)

        pawns =
            List.repeat 8
                (Pawn False)

        majorPieces =
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
            List.map (\pieceType -> { color = color, pieceType = pieceType }) pieces
                |> List.map2 Tuple.pair (row index)

        allPieces =
            rowPieces 1 White majorPieces
                ++ rowPieces 2 White pawns
                ++ rowPieces 7 Black pawns
                ++ rowPieces 8 Black majorPieces

        insertPiece ( cell, piece ) board =
            Dict.insert cell piece board
    in
    List.foldl insertPiece Dict.empty allPieces


playMove : Game -> Move -> Game
playMove (Game game) move =
    let
        nextPlayer =
            oppositeColor game.player

        newBoard =
            pieceAt game.board move.src
                |> Maybe.map (\piece -> Dict.insert move.dst piece game.board)
                |> Maybe.map (Dict.remove move.src)
                |> Maybe.withDefault game.board
    in
    Game
        { game
            | player = nextPlayer
            , board = newBoard
            , gameHistory = game.gameHistory ++ [ Game game ]
            , lastMove = Just move
            , isCheck = isCheck newBoard nextPlayer
            , isCheckmate = False
        }


isThreatenedBy : Board -> Cell -> Color -> Bool
isThreatenedBy board target color =
    let
        getCaptures =
            pieceCaptures board target
    in
    Dict.values board
        |> List.filter ((==) color << .color)
        |> List.concatMap getCaptures
        |> List.any ((==) target << .dst)


isCheck : Board -> Color -> Bool
isCheck board color =
    let
        isKing piece =
            case piece.pieceType of
                King _ ->
                    True

                _ ->
                    False

        isChecked_ cell =
            isThreatenedBy board cell (oppositeColor color)
    in
    Dict.filter (\_ piece -> isKing piece && piece.color == color) board
        |> (List.head << Dict.keys)
        |> Maybe.unwrap False isChecked_


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


nextCell : Cell -> Direction -> Maybe Cell
nextCell ( srcRow, srcCol ) ( row, col ) =
    fromInt { row = srcRow + row, col = srcCol + col }


followDirection : Cell -> Direction -> List Cell
followDirection start dir =
    let
        doRepeat c =
            c :: followDirection c dir
    in
    nextCell start dir
        |> Maybe.unwrap [] doRepeat


capture : Board -> Color -> Cell -> Direction -> Maybe Cell
capture board color start dir =
    let
        isOpponent =
            isColor (oppositeColor color) board
    in
    nextCell start dir
        |> Maybe.filter isOpponent


captureDirection : Board -> Color -> Cell -> Direction -> Maybe Cell
captureDirection board color start dir =
    let
        isValidTarget =
            isColor (oppositeColor color) board
    in
    followDirection start dir
        |> List.dropWhile (isEmpty board)
        |> List.head
        |> Maybe.filter isValidTarget


captureAllDirections : Board -> Cell -> Color -> List Direction -> List Cell
captureAllDirections board start color dirs =
    List.filterMap (captureDirection board color start) dirs


step : Board -> Cell -> Direction -> Maybe Cell
step board start dir =
    nextCell start dir
        |> Maybe.filter (isEmpty board)


stepDirection : Board -> Cell -> Direction -> List Cell
stepDirection board start dir =
    step board start dir
        |> Maybe.map (\c -> c :: stepDirection board c dir)
        |> Maybe.withDefault []


stepAllDirections : Board -> Cell -> List Direction -> List Cell
stepAllDirections board start dirs =
    List.concatMap (stepDirection board start) dirs



-- PIECE MOVES --


toStandardMove : Cell -> Cell -> Move
toStandardMove src dst =
    { src = src, dst = dst, moveType = Standard }


toPawnStartMove : Cell -> Cell -> Move
toPawnStartMove src dst =
    { src = src, dst = dst, moveType = PawnStart }


pawnForward : Color -> Direction
pawnForward color =
    if color == White then
        ( 1, 0 )

    else
        ( -1, 0 )


rookDirections : List Direction
rookDirections =
    [ ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 0, 1 )
    , ( 0, 1 )
    , ( -1, 1 )
    , ( -1, 0 )
    , ( -1, -1 )
    ]


knightDirections : List Direction
knightDirections =
    List.uniquePairs [ 2, 1 ]
        ++ List.uniquePairs [ 2, -1 ]
        ++ List.uniquePairs [ -2, 1 ]
        ++ List.uniquePairs [ -2, -1 ]


bishopDirections : List Direction
bishopDirections =
    [ ( 1, 1 )
    , ( 1, -1 )
    , ( -1, 1 )
    , ( -1, -1 )
    ]


kingDirections : List Direction
kingDirections =
    bishopDirections ++ rookDirections


queenDirections : List Direction
queenDirections =
    kingDirections


rookSteps : Board -> Cell -> List Move
rookSteps board start =
    stepAllDirections board start rookDirections
        |> List.map (toStandardMove start)


rookCaptures : Board -> Cell -> Color -> List Move
rookCaptures board start color =
    captureAllDirections board start color rookDirections
        |> List.map (toStandardMove start)


knightSteps : Board -> Cell -> List Move
knightSteps board start =
    List.map (step board start) knightDirections
        |> Maybe.values
        |> List.map (toStandardMove start)


knightCaptures : Board -> Cell -> Color -> List Move
knightCaptures board start color =
    List.map (capture board color start) knightDirections
        |> Maybe.values
        |> List.map (toStandardMove start)


bishopSteps : Board -> Cell -> List Move
bishopSteps board start =
    stepAllDirections board start bishopDirections
        |> List.map (toStandardMove start)


bishopCaptures : Board -> Cell -> Color -> List Move
bishopCaptures board start color =
    captureAllDirections board start color bishopDirections
        |> List.map (toStandardMove start)


queenSteps : Board -> Cell -> List Move
queenSteps board start =
    stepAllDirections board start queenDirections
        |> List.map (toStandardMove start)


queenCaptures : Board -> Cell -> Color -> List Move
queenCaptures board start color =
    captureAllDirections board start color queenDirections
        |> List.map (toStandardMove start)


kingCaptures : Board -> Cell -> Color -> List Move
kingCaptures board start color =
    kingDirections
        |> List.map (capture board color start)
        |> Maybe.values
        |> List.map (toStandardMove start)


kingSteps : Board -> Cell -> List Move
kingSteps board start =
    kingDirections
        |> List.map (step board start)
        |> Maybe.values
        |> List.map (toStandardMove start)


pawnSteps : Board -> Cell -> Color -> Bool -> List Move
pawnSteps board start color hasMoved =
    let
        moveNb =
            if hasMoved then
                1

            else
                2

        pawnDir =
            pawnForward color

        pawnStep =
            step board start pawnDir
                |> Maybe.map (toStandardMove start)

        pawnStart =
            pawnStep
                |> Maybe.andThen
                    (\move ->
                        step board move.dst pawnDir
                    )
                |> Maybe.map (toPawnStartMove start)
    in
    [ pawnStep, pawnStart ]
        |> List.take moveNb
        |> Maybe.values


pawnCaptures : Board -> Cell -> Color -> List Move
pawnCaptures board start color =
    let
        captureDirections =
            if color == White then
                [ ( 1, 1 ), ( 1, -1 ) ]

            else
                [ ( -1, 1 ), ( -1, -1 ) ]
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
            nextCell opponentCell (pawnForward color)

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


pieceCaptures : Board -> Cell -> Piece -> List Move
pieceCaptures board cell piece =
    case piece.pieceType of
        Rook ->
            rookCaptures board cell piece.color

        Knight ->
            knightCaptures board cell piece.color

        Bishop ->
            bishopCaptures board cell piece.color

        King _ ->
            kingCaptures board cell piece.color

        Queen ->
            queenCaptures board cell piece.color

        Pawn _ ->
            pawnCaptures board cell piece.color


pieceSteps : Board -> Cell -> Piece -> List Move
pieceSteps board cell piece =
    case piece.pieceType of
        Rook ->
            rookSteps board cell

        Knight ->
            knightSteps board cell

        Bishop ->
            bishopSteps board cell

        King _ ->
            kingSteps board cell

        Queen ->
            queenSteps board cell

        Pawn hasMoved ->
            pawnSteps board cell piece.color hasMoved



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
