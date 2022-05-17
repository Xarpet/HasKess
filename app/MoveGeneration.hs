{-# LANGUAGE RecordWildCards #-}
module MoveGeneration where

import GameState
import Move
import FEN
import Bitboard

import Data.Bits
import Data.Word


legalMoves :: StateComplex -> [Move]
legalMoves sc@(StateComplex GameState{..} bs) =
    concatMap (uncurry legal . (\x -> (x,fromJust $ testSquareFromBitboard bs x))) allyIndex
    where
        legal = legalMovePiece sc
        allyIndex = bitboardToFlippedIndex $ colorInBitboardState activeColor bs
        fromJust (Just a) = a

legalMovePiece :: StateComplex -> Int -> Piece -> [Move]
legalMovePiece sc@(StateComplex GameState{..} bs) from piece@(Piece Pawn color) =
    filter legalQ allMove
    where
        legalQ m = moveLegality m sc
        attack = bitboardToFlippedIndex $ singlePieceAttackSquares piece bs from
        move = bitboardToFlippedIndex $ singlePieceMoveSquares piece bs from
        allMove -- when at one to last rank can promote
            | (activeColor == White && testBit (71776119061217280 :: Word64) from) ||
                 (activeColor == Black && testBit (65280 :: Word64) from) =
                map relocate (attack ++ move) ++ map enPassant attack ++
                concatMap promote (attack ++ move)
            | otherwise =
                map relocate (attack ++ move) ++ map enPassant attack
        relocate = Move Relocate piece from
        enPassant = Move EnPassant piece from
        promote to = [Move Promote (Piece Queen activeColor) from to,
                      Move Promote (Piece Rook activeColor) from to,
                      Move Promote (Piece Bishop activeColor) from to,
                      Move Promote (Piece Knight activeColor) from to]

legalMovePiece sc@(StateComplex GameState{..} bs) from piece@(Piece King color) =
    filter legalQ allMoves
    where
        legalQ m = moveLegality m sc
        move = bitboardToFlippedIndex $ singlePieceMoveSquares piece bs from
        relocate = Move Relocate piece from
        allMoves = Move Castle piece from (from + 2): -- to cover the castling
                   Move Castle piece from (from - 2): map relocate move


legalMovePiece sc@(StateComplex GameState{..} bs) from piece@(Piece kind color) =
    filter legalQ $ map relocate move
    where
        legalQ m = moveLegality m sc
        move = bitboardToFlippedIndex $ singlePieceMoveSquares piece bs from
        relocate = Move Relocate piece from
