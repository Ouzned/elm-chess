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


type alias CellMove =
    Cell -> Maybe Cell


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
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft


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


move : Cell -> Cell -> Board -> Board
move src dst board =
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
        |> Maybe.map sameColor
        |> Maybe.withDefault False


opposite : Color -> Color
opposite color =
    if color == White then
        Black

    else
        White


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


stepOrCapture : Board -> Cell -> Color -> CellMove -> Maybe Cell
stepOrCapture board src color moveCell =
    let
        otherColorOrEmpty =
            not << isColor color board

        nextCell =
            moveCell src
    in
    Maybe.filter otherColorOrEmpty nextCell


moveOrCapture : Board -> Cell -> Color -> CellMove -> List Cell
moveOrCapture board cell color moveCell =
    let
        next =
            stepOrCapture board cell color moveCell

        repeat c =
            moveOrCapture board c color moveCell

        hasOpponent =
            hasPiece board
    in
    case next of
        Nothing ->
            []

        Just nextCell ->
            if hasOpponent nextCell then
                [ nextCell ]

            else
                nextCell :: repeat nextCell


step : Board -> Cell -> CellMove -> Maybe Cell
step board src moveCell =
    Maybe.filter (isEmpty board) (moveCell src)


stepN : Board -> Cell -> CellMove -> Int -> List Cell
stepN board src moveCell nb =
    if nb == 0 then
        []

    else
        case step board src moveCell of
            Nothing ->
                []

            Just c ->
                c :: stepN board c moveCell (nb - 1)


capture : Board -> Cell -> Color -> CellMove -> Maybe Cell
capture board src color moveCell =
    let
        isOpponent =
            isColor (opposite color) board
    in
    Maybe.filter isOpponent (moveCell src)


toStandardMove : Cell -> Cell -> Move
toStandardMove src dst =
    { src = src, dst = dst, moveType = Standard }


directionsToMoves : List CellMove -> Board -> Cell -> Color -> List Move
directionsToMoves directions board src color =
    let
        action =
            moveOrCapture board src color
    in
    List.concatMap action directions
        |> List.map (toStandardMove src)


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


knightMoves : Board -> Cell -> Color -> List Move
knightMoves board src color =
    let
        moves =
            [ [ up, up, left ]
            , [ up, up, right ]
            , [ up, right, right ]
            , [ up, left, left ]
            , [ down, down, right ]
            , [ down, down, left ]
            , [ down, left, left ]
            , [ down, right, right ]
            ]

        doMoves list =
            moveCells list src

        otherColorOrEmpty =
            not << isColor color board
    in
    List.filterMap doMoves moves
        |> List.filter otherColorOrEmpty
        |> List.map (toStandardMove src)


bishopMoves : Board -> Cell -> Color -> List Move
bishopMoves =
    directionsToMoves
        [ upRight, upLeft, downRight, downLeft ]


queenMoves : Board -> Cell -> Color -> List Move
queenMoves =
    directionsToMoves
        [ up
        , down
        , left
        , right
        , upRight
        , upLeft
        , downRight
        , downLeft
        ]


rookMoves : Board -> Cell -> Color -> List Move
rookMoves =
    directionsToMoves
        [ up, down, right, left ]


pawnMoves : Board -> Cell -> Color -> Bool -> List Move
pawnMoves board src color hasMoved =
    let
        moveAction =
            pawnForward color

        captureActions =
            pawnCaptures color

        moveNb =
            pawnMoveNb hasMoved

        steps =
            stepN board src moveAction moveNb

        captures =
            List.filterMap (capture board src color) captureActions
    in
    List.map (toStandardMove src) (steps ++ captures)


enPassant : Cell -> Color -> Move -> Maybe Move
enPassant pawnCell color prevMove =
    let
        opponent =
            prevMove.dst

        moveBelow =
            pawnForward color

        toMove dest =
            Just <|
                { src = pawnCell
                , dst = dest
                , moveType =
                    EnPassant opponent
                }
    in
    case prevMove.moveType of
        PawnStart ->
            if isNextTo pawnCell opponent then
                Maybe.andThen toMove (moveBelow opponent)

            else
                Nothing

        _ ->
            Nothing


down : CellMove
down ( row, col ) =
    fromInt { row = row - 1, col = col }


up : CellMove
up ( row, col ) =
    fromInt { row = row + 1, col = col }


left : CellMove
left ( row, col ) =
    fromInt { row = row, col = col - 1 }


right : CellMove
right ( row, col ) =
    fromInt { row = row, col = col + 1 }


upLeft : CellMove
upLeft =
    moveCells [ up, left ]


upRight : CellMove
upRight =
    moveCells [ up, right ]


downLeft : CellMove
downLeft =
    moveCells [ down, left ]


downRight : CellMove
downRight =
    moveCells [ down, right ]


isNextTo : Cell -> Cell -> Bool
isNextTo ( row, col ) ( row_, col_ ) =
    row == row_ && abs (col - col_) == 1


moveCells : List CellMove -> Cell -> Maybe Cell
moveCells moves src =
    List.foldl Maybe.andThen (Just src) moves


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


pawnMoveNb : Bool -> Int
pawnMoveNb hasMoved =
    if hasMoved then
        1

    else
        2


pawnCaptures : Color -> List CellMove
pawnCaptures color =
    if color == White then
        [ upRight, upLeft ]

    else
        [ downRight, downLeft ]


pawnForward : Color -> CellMove
pawnForward color =
    if color == White then
        up

    else
        down



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
