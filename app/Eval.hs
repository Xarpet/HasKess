{-# LANGUAGE BangPatterns #-}
module Eval where

import NNUE ( arrayEval )
import Bitboard
    ( BitboardState,
      StateComplex(StateComplex),
      pieceInBitboardState,
      totalValue,
      bitboardToFlippedIndexCInt )
import GameState
    ( Color(..),
      PieceType(Queen, King, Pawn, Knight, Bishop, Rook),
      Piece(Piece),
      GameState(GameState, activeColor),
      opponent )
import Move ( complexInCheck )
import MoveGeneration ( legalMoves )

import Foreign.C.Types
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr
import GHC.IO (unsafePerformIO)


data GameResult = CheckMate | StaleMate | Continue
    deriving (Eq, Show)

getGameResult :: StateComplex -> GameResult
getGameResult sc
    | complexInCheck sc && null (legalMoves sc) = CheckMate
    | null (legalMoves sc) = StaleMate
    | otherwise = Continue

-- transform the board to CArray

pieceCode :: Piece -> CInt
pieceCode (Piece King White) = 1
pieceCode (Piece Queen White) = 2
pieceCode (Piece Rook White) = 3
pieceCode (Piece Bishop White) = 4
pieceCode (Piece Knight White) = 5
pieceCode (Piece Pawn White) = 6
pieceCode (Piece King Black) = 7
pieceCode (Piece Queen Black) = 8
pieceCode (Piece Rook Black) = 9
pieceCode (Piece Bishop Black) = 10
pieceCode (Piece Knight Black) = 11
pieceCode (Piece Pawn Black) = 12

colorToCInt :: Color -> CInt
colorToCInt White = 0
colorToCInt Black = 1

bsToList :: BitboardState -> ([CInt],[CInt])
bsToList bs = (pieces, squares) where
    (pieces,squares) = unzip $ concatMap turnPieceToList
        [Piece King White,
         Piece King Black,
         Piece Pawn White,
         Piece Knight White,
         Piece Bishop White,
         Piece Rook White,
         Piece Queen White,
         Piece Pawn Black,
         Piece Knight Black,
         Piece Bishop Black,
         Piece Rook Black,
         Piece Queen Black]
    turnPieceToList p = zip (repeat (pieceCode p)) $ bitboardToFlippedIndexCInt (pieceInBitboardState p bs)
    -- maybe it would be faster to generate this from Board. But whatever

eval :: StateComplex -> Int
eval sc@(StateComplex GameState{activeColor = color} bs)
    | res == CheckMate = minBound
    | res == StaleMate = 0
    | otherwise = fromEnum $ unsafePerformIO $ arrayEval (colorToCInt color) pieces squares
        where
            res = getGameResult sc
            (p,s) = bsToList bs
            pieces = VS.fromList $ p ++ [0]
            squares = VS.fromList s
{-# INLINE eval #-}



eval' :: StateComplex -> Int
eval' (StateComplex GameState{activeColor = color} bs) =
    totalValue color bs - totalValue (opponent color) bs

-- interface
-- /**
-- * Evaluation subroutine suitable for chess engines.
-- * -------------------------------------------------
-- * Piece codes are
-- *     wking=1, wqueen=2, wrook=3, wbishop= 4, wknight= 5, wpawn= 6,
-- *     bking=7, bqueen=8, brook=9, bbishop=10, bknight=11, bpawn=12,
-- * Squares are
-- *     A1=0, B1=1 ... H8=63
-- * Input format:
-- *     piece[0] is white king, square[0] is its location
-- *     piece[1] is black king, square[1] is its location
-- *     ..
-- *     piece[x], square[x] can be in any order
-- *     ..
-- *     piece[n+1] is set to 0 to represent end of array
-- * Returns
-- *   Score relative to side to move in approximate centi-pawns
-- */
-- DLLExport int _CDECL nnue_evaluate(
--   int player,                       /** Side to move: white=0 black=1 */
--   int* pieces,                      /** Array of pieces */
--   int* squares                      /** Corresponding array of squares each piece stands on */
--