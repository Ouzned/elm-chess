module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (dir, src, target)
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
    , position : Cell
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
    { piece : Piece
    , dst : Cell
    , moveType : MoveType
    }


type MoveType
    = Standard
    | PawnStart
    | EnPassant Piece
    | Castling
        { rook : Cell
        , rookDst : Cell
        }


type CastlingType
    = CastlingQueen
    | CastlingKing


type alias Direction =
    ( Row, Col )



-- GAME FUNCTIONS --


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


availableMoves : Game -> List Move
availableMoves (Game game) =
    let
        simulateMove move =
            ( move, playMove (Game game) move )

        isMovePossible ( _, Game simulation ) =
            not (isCheck simulation.board simulation.player)
    in
    game.board
        |> Dict.filter
            (\_ piece ->
                piece.color == game.player
            )
        |> Dict.map
            (\cell piece ->
                pieceCaptures game.board piece
                    ++ pieceSteps game.board piece
            )
        |> (List.concat << Dict.values)
        |> List.map simulateMove
        |> List.filter isMovePossible
        |> List.map Tuple.first


playMove : Game -> Move -> Game
playMove (Game game) move =
    let
        nextPlayer =
            oppositeColor game.player

        newBoard =
            applyMove game.board move
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


applyMove : Board -> Move -> Board
applyMove board move =
    case move.moveType of
        EnPassant target ->
            movePiece board move.piece move.dst
                |> (\newBoard -> removePiece newBoard target)

        _ ->
            movePiece board move.piece move.dst



-- BOARD FUNCTIONS --


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
            List.map2
                (\pieceType pos ->
                    { color = color, pieceType = pieceType, position = pos }
                )
                pieces
                (row index)

        allPieces =
            rowPieces 1 White majorPieces
                ++ rowPieces 2 White pawns
                ++ rowPieces 7 Black pawns
                ++ rowPieces 8 Black majorPieces

        insertPiece piece board =
            Dict.insert piece.position piece board
    in
    List.foldl insertPiece Dict.empty allPieces


isThreatenedBy : Board -> Cell -> Color -> Bool
isThreatenedBy board target color =
    let
        getCaptures =
            pieceCaptures board
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
    board
        |> Dict.filter
            (\_ piece ->
                isKing piece && piece.color == color
            )
        |> (List.head << Dict.keys)
        |> Maybe.unwrap False isChecked_


pieceAt : Board -> Cell -> Maybe Piece
pieceAt board cell =
    Dict.get cell board


isEmpty : Board -> Cell -> Bool
isEmpty board cell =
    pieceAt board cell == Nothing


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


movePiece : Board -> Piece -> Cell -> Board
movePiece board piece target =
    let
        newPiece =
            { piece | position = target }
    in
    removePiece board piece
        |> Dict.insert target newPiece


removePiece : Board -> Piece -> Board
removePiece board piece =
    Dict.remove piece.position board



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


nextCell : Cell -> Direction -> Maybe Cell
nextCell ( srcRow, srcCol ) ( row, col ) =
    fromInt
        { row = srcRow + row
        , col = srcCol + col
        }


followDirection : Cell -> Direction -> List Cell
followDirection start dir =
    let
        doRepeat c =
            c :: followDirection c dir
    in
    nextCell start dir
        |> Maybe.unwrap [] doRepeat


nextEmptyCell : Board -> Cell -> Direction -> Maybe Cell
nextEmptyCell board cell dir =
    nextCell cell dir
        |> Maybe.filter (isEmpty board)


nextColorCell : Board -> Cell -> Color -> Direction -> Maybe Cell
nextColorCell board cell color dir =
    nextCell cell dir
        |> Maybe.filter (isColor color board)


findColorCell : Board -> Cell -> Color -> Direction -> Maybe Cell
findColorCell board start color dir =
    followDirection start dir
        |> List.dropWhile (isEmpty board)
        |> List.head
        |> Maybe.filter (isColor color board)


findEmptyCells : Board -> Cell -> Direction -> List Cell
findEmptyCells board start dir =
    followDirection start dir
        |> List.takeWhile (isEmpty board)



-- PIECE DIRECTION --


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


pawnCaptureDirections : Color -> List Direction
pawnCaptureDirections color =
    if color == White then
        [ ( 1, 1 ), ( 1, -1 ) ]

    else
        [ ( -1, 1 ), ( -1, -1 ) ]


pawnMoveDirection : Color -> Direction
pawnMoveDirection color =
    if color == White then
        ( 1, 0 )

    else
        ( -1, 0 )


pawnMoveNb : Bool -> Int
pawnMoveNb hasMoved =
    if hasMoved then
        1

    else
        2



-- PIECE MOVES --


toStandardMove : Piece -> Cell -> Move
toStandardMove piece dst =
    { piece = piece
    , dst = dst
    , moveType = Standard
    }


toPawnStartMove : Piece -> Cell -> Move
toPawnStartMove piece dst =
    { piece = piece
    , dst = dst
    , moveType = PawnStart
    }


toEnPassantMove : Piece -> Piece -> Cell -> Move
toEnPassantMove piece opponent dst =
    { piece = piece
    , dst = dst
    , moveType = EnPassant opponent
    }


unThreatenedMoves : Board -> Color -> List Move -> List Move
unThreatenedMoves board color moves =
    List.filter
        (\{ dst } ->
            not (isThreatenedBy board dst color)
        )
        moves


pieceCaptures : Board -> Piece -> List Move
pieceCaptures board piece =
    let
        opponentColor =
            oppositeColor piece.color

        captureDirs dirs =
            List.filterMap (findColorCell board piece.position opponentColor) dirs
                |> List.map (toStandardMove piece)

        captureNext dirs =
            List.filterMap (nextColorCell board piece.position opponentColor) dirs
                |> List.map (toStandardMove piece)
    in
    case piece.pieceType of
        Rook ->
            captureDirs rookDirections

        Knight ->
            captureNext knightDirections

        Bishop ->
            captureDirs bishopDirections

        King _ ->
            captureNext kingDirections

        Queen ->
            captureDirs queenDirections

        Pawn _ ->
            captureNext (pawnCaptureDirections piece.color)


pieceSteps : Board -> Piece -> List Move
pieceSteps board piece =
    let
        opponentColor =
            oppositeColor piece.color

        stepNext dirs =
            List.filterMap (nextEmptyCell board piece.position) dirs
                |> List.map (toStandardMove piece)

        stepDirs dirs =
            List.concatMap (findEmptyCells board piece.position) dirs
                |> List.map (toStandardMove piece)

        pawnFirstStep dir =
            nextEmptyCell board piece.position dir
                |> Maybe.map (toStandardMove piece)

        pawnStart dir =
            pawnFirstStep dir
                |> Maybe.andThen (\{ dst } -> nextEmptyCell board dst dir)
                |> Maybe.map (toPawnStartMove piece)
    in
    case piece.pieceType of
        Rook ->
            stepDirs rookDirections

        Knight ->
            stepNext knightDirections

        Bishop ->
            stepDirs bishopDirections

        King _ ->
            stepNext kingDirections

        Queen ->
            stepDirs queenDirections

        Pawn hasMoved ->
            [ pawnFirstStep, pawnStart ]
                |> List.andMap [ pawnMoveDirection piece.color ]
                |> Maybe.values
                |> List.take (pawnMoveNb hasMoved)



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
