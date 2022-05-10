module Bitboard where

import GameState
import FEN
import Data.Word
import Data.Bits


type Bitboard = Word64

emptyBitboard :: Bitboard
emptyBitboard = 0

data BitboardState = BitboardState {
    whitePawn :: Bitboard,
    whiteKnight :: Bitboard,
    whiteBishop :: Bitboard,
    whiteRook :: Bitboard,
    whiteQueen :: Bitboard,
    whiteKing :: Bitboard,
    blackPawn :: Bitboard,
    blackKnight :: Bitboard,
    blackBishop :: Bitboard,
    blackRook :: Bitboard,
    blackQueen :: Bitboard,
    blackKing :: Bitboard,
    whiteTotal :: Bitboard,
    blackTotal :: Bitboard,
    total :: Bitboard
} deriving (Eq, Show)
-- we go a1, b1 ... h8.

intersectBitboard :: Bitboard -> Bitboard -> Bitboard
intersectBitboard = (.&.) -- bitwise and

unionBitboard :: Bitboard -> Bitboard -> Bitboard
unionBitboard = (.|.) -- bitwise or

totalValue :: Color -> BitboardState -> Int
totalValue White board = popCount (whitePawn board) +
                         3 * popCount (whiteBishop board) +
                         3 * popCount (whiteKnight board) +
                         5 * popCount (whiteRook board) +
                         9 * popCount (whiteQueen board)
totalValue Black board = popCount (blackPawn board) +
                         3 * popCount (blackBishop board) +
                         3 * popCount (blackKnight board) +
                         5 * popCount (blackRook board) +
                         9 * popCount (blackQueen board)

boardToBitboard :: Board -> BitboardState
boardToBitboard board = BitboardState
    {
        whitePawn = whitePawn,
        whiteKnight = whiteKnight,
        whiteBishop = whiteBishop,
        whiteRook = whiteRook,
        whiteQueen = whiteQueen,
        whiteKing = whiteKing,
        blackPawn = blackPawn,
        blackKnight = blackKnight,
        blackBishop = blackBishop,
        blackRook = blackRook,
        blackQueen = blackQueen,
        blackKing = blackKing,
        whiteTotal = whiteTotal,
        blackTotal = blackTotal,
        total = total
    }
    where
        whitePawn = pieceToBitboard (Piece Pawn White) board
        whiteKnight = pieceToBitboard (Piece Knight White) board
        whiteBishop = pieceToBitboard (Piece Bishop White) board
        whiteRook = pieceToBitboard (Piece Rook White) board
        whiteQueen = pieceToBitboard (Piece Queen White) board
        whiteKing = pieceToBitboard (Piece King White) board
        blackPawn = pieceToBitboard (Piece Pawn Black) board
        blackKnight = pieceToBitboard (Piece Knight Black) board
        blackBishop = pieceToBitboard (Piece Bishop Black) board
        blackRook = pieceToBitboard (Piece Rook Black) board
        blackQueen = pieceToBitboard (Piece Queen Black) board
        blackKing = pieceToBitboard (Piece King Black) board
        whiteTotal = foldr unionBitboard zeroBits [whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen, whiteKing]
        blackTotal = foldr unionBitboard zeroBits [blackPawn, blackKnight, blackBishop, blackRook, blackQueen, blackKing]
        total = unionBitboard whiteTotal blackTotal

        pieceToBitboard :: Piece -> Board -> Bitboard
        pieceToBitboard piece board = foldl (ifEQPieceThenFlipBit piece) zeroBits $ zip [0..63] $ concat board

        ifEQPieceThenFlipBit :: Piece -> Bitboard -> (Int, Maybe Piece) -> Bitboard
        ifEQPieceThenFlipBit piece bitboard (index, maybePiece)
            | maybePiece == Just piece = bitboard `setBit` index
            | otherwise = bitboard

fENToBitboard :: String -> BitboardState
fENToBitboard = boardToBitboard . board . parserFEN
