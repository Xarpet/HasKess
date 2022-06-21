{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module NewMoveGen where

import Data.List ( unfoldr )
import Bitboard (showFEN, StateComplex (StateComplex), testSquareFromBitboard)
import Foreign.C.String ( newCString, CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal.Array ( peekArray0 )
import System.IO.Unsafe ( unsafePerformIO )
import Move (Move(..), MoveType(..))
import GameState (GameState(GameState), Piece(..), PieceType(..), Color(..))

foreign import ccall unsafe "parseFEN" fenmoves :: CString -> Ptr CInt
foreign import ccall unsafe "init_magic" initMagic :: IO ()

peekFEN :: CString -> [CInt]
peekFEN s = unsafePerformIO $ peekArray0 (-1) (fenmoves s)

newLegalMovesFEN :: StateComplex -> [Move]
newLegalMovesFEN sc = cIntToMoves sc $ peekFEN $ unsafePerformIO $ newCString $ showFEN sc
    where
        cIntToMoves :: StateComplex -> [CInt] -> [Move]
        cIntToMoves sc list =
            map (fromtoMoves sc) (splitn 2 $ map fromEnum list)
        fromtoMoves sc@(StateComplex GameState{..} bs) [from, to] = case testSquareFromBitboard bs from of
            Just piece@(Piece Pawn White) | to > 55 -> Move Promote (Piece Queen White) from to
            Just piece@(Piece Pawn Black) | to <8 -> Move Promote (Piece Queen Black) from to
            Just piece@(Piece Pawn _) | abs (from - to) /= 8 && abs (from - to) /= 16 && testSquareFromBitboard bs to == Nothing ->
                Move EnPassant piece from to
            Just piece@(Piece King _) | abs (from - to) == 2 -> Move Castle piece from to
            Just piece -> Move Relocate piece from to
            Nothing -> error "No mover"
        fromtoMoves _ list = error $ show list

        splitn :: Int -> [a] -> [[a]] -- splits a list
        splitn n = unfoldr (\s -> if null s then Nothing else Just $ splitAt n s)