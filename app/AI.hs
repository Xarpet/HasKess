{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BangPatterns #-}
module AI where

import Eval ( eval )
import MoveGeneration ( legalMoves )
import Bitboard ( StateComplex )
import Move ( Move, moveComplex )

newtype MoveValue = MoveValue (Move, Int)
    deriving Eq
-- implemented: 
-- minimax
-- alpha-beta pruning
-- yet implemented:
-- Quiescence Search
-- principal variation search
-- null move pruning

instance Ord MoveValue where
    (MoveValue (move1, i1)) <= MoveValue (move2, i2) = i1 <= i2

instance Show MoveValue where
    show (MoveValue (move,i)) = show move ++ show i

minimax :: Int -> Int -> Int -> StateComplex -> Int -- white wants to maximize the value, while black want to minimize.
minimax 0 alpha beta sc =
    alpha `max` eval sc `min` beta -- we convert to white's centipawn
minimax depth alpha beta sc =
    case legalMoves sc of
        [] -> minimax 0 alpha beta sc
        list -> cmx alpha beta $ map (moveComplex sc) list
        where
            cmx a b [] = a
            cmx a b (t:ts)
                | a' >= b   = a'
                | otherwise = cmx a' b ts where
                    a' = - minimax (depth - 1) (-b) (-a) t
-- This is actually 

choose :: Int -> StateComplex -> MoveValue
choose !depth !sc =
    minimum values
    where
        values :: [MoveValue]
        values = map (\x -> MoveValue (x,(minimax depth minBound maxBound . moveComplex sc) x)) $ legalMoves sc
{-# INLINE choose #-}