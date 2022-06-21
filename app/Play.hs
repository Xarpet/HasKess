{-# LANGUAGE BangPatterns #-}
module Play where

import GameState ( showBoard )
import Bitboard
    ( StateComplex(..), bitboardToBoard, fENToComplex, showFEN )
import Move ( Move, complexInCheck, moveAction, readAN )
import Eval ( eval )
import NNUE ( initializeNNUE )
import AI ( MoveValue(..), choose )

initialSC :: StateComplex
initialSC = fENToComplex "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

testSC :: StateComplex
testSC = fENToComplex "r3k1nr/ppp2ppp/5q2/2bP4/2Bn2b1/2P1B3/PP3PPP/RN1QK2R w KQkq - 1 9"


displayInCheck :: Bool -> String
displayInCheck True = "\ESC[33mYou are in check. You must get out of check\ESC[0m"
displayInCheck False = ""

move :: MoveValue -> Move
move (MoveValue (m,i)) = m

fromJust :: Maybe a -> a
fromJust (Just a) = a

play :: StateComplex -> IO ()
play sc@(StateComplex gs bs) = do
    putStrLn $ showBoard $ bitboardToBoard bs
    print $ gameState sc
    putStrLn ("FEN: " ++ showFEN sc)
    putStrLn "\n---------------------------------\n"
    putStrLn $ displayInCheck $ complexInCheck sc
    let mv = choose 6 sc
    print mv
    let sc1 = fromJust $ moveAction sc $ move mv
    putStrLn $ showBoard $ bitboardToBoard $ bitboardState sc1
    print $ gameState sc1
    putStrLn ("FEN: " ++ showFEN sc1)
    putStrLn "\n---------------------------------\n"
    putStrLn $ displayInCheck $ complexInCheck sc1

    putStrLn "Your next move:"
    move <- getLine
    if move == "exit"
        then return ()
        else do
            let newsc = moveAction sc1 $ readAN move sc1
            case newsc of
                Nothing -> do
                    putStrLn "\ESC[31mIllegal move. try again \n---------------------------------------------\ESC[0m"
                    play sc
                Just a -> play a
{-# INLINE play #-}

testEval :: IO ()
testEval = do
    a <- initializeNNUE
    print a
    print $ eval testSC