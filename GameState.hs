module GameState
    where

data GameState = GameState
    {
        board :: Board,
        activeColor :: Color,
        castlable :: Castlable,
        enPassantSquare :: Maybe Coordinate,
        halfMoveClock :: Int, --used to implement the fifty move rule
        fullMoveNumber :: Int
    } --Implement the FEN notation of chess

type Board = [[Square]]
type Square = Maybe Piece
data Piece = Piece {
    pieceType :: PieceType,
    pieceColor :: Color
} deriving Eq

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving Eq

data Color = Black | White
    deriving (Show, Eq)

data Castlable = Castlable {
    whiteKingSide :: Bool,
    whiteQueenSide :: Bool,
    blackKingSide :: Bool,
    blackQueenSide :: Bool
} deriving (Show, Eq)

newtype Coordinate = Coordinate (Char, Int) -- in algerbraic notation
    deriving (Show, Eq)

instance Show Piece where
    show (Piece Pawn White) = "P"
    show (Piece Knight White) = "N"
    show (Piece Bishop White)= "B"
    show (Piece Rook White)= "R"
    show (Piece Queen White)= "Q"
    show (Piece King White)= "K"

    show (Piece Pawn Black)= "p"
    show (Piece Knight Black)= "n"
    show (Piece Bishop Black)= "b"
    show (Piece Rook Black)= "r"
    show (Piece Queen Black)= "q"
    show (Piece King Black)= "k"

readFENPiece :: Char -> Either Piece Int
readFENPiece 'P' = Left (Piece Pawn White)
readFENPiece 'N' = Left (Piece Knight White)
readFENPiece 'B' = Left (Piece Bishop White)
readFENPiece 'R' = Left (Piece Rook White)
readFENPiece 'Q' = Left (Piece Queen White)
readFENPiece 'K' = Left (Piece King White)
readFENPiece 'p' = Left (Piece Pawn Black)
readFENPiece 'n' = Left (Piece Knight Black)
readFENPiece 'b' = Left (Piece Bishop Black)
readFENPiece 'r' = Left (Piece Rook Black)
readFENPiece 'q' = Left (Piece Queen Black)
readFENPiece 'k' = Left (Piece King Black)
readFENPiece num = Right $ (read :: String -> Int) [num]
-- instance read is too weird so this is a custom read function

showBoard :: Board -> String
showBoard = concatMap ((++) "\n" . map (maybe '-' $ head . show)) . reverse
-- replace '-' if you want other placeholder
-- Note the reverse

instance Show GameState where
    show (GameState board colorToPlay castlable enPassantSquare halfMoveClock fullMoveNumber) =
        showBoard board ++ "\n" ++
        show colorToPlay ++ " to move\n" ++
        show castlable ++ "\nEn Passant Square: " ++
        show enPassantSquare ++ "\nHalf Move Clock: " ++
        show halfMoveClock ++ "\nTotal Move: " ++
        show fullMoveNumber

initialGameState :: GameState
initialGameState = GameState [[Just (Piece Rook White),Just (Piece Knight White),Just (Piece Bishop White),Just (Piece Queen White),Just (Piece King White),Just (Piece Bishop White),Just (Piece Knight White),Just (Piece Rook White)],
                              [Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White),Just (Piece Pawn White)],
                              [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                              [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                              [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                              [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                              [Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black),Just (Piece Pawn Black)],
                              [Just (Piece Rook Black),Just (Piece Knight Black),Just (Piece Bishop Black),Just (Piece Queen Black),Just (Piece King Black),Just (Piece Bishop Black),Just (Piece Knight Black),Just (Piece Rook Black)]]
                             White (Castlable True True True True) Nothing 0 1
-- note now we start from a1 to h8.
