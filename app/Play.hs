module Play where

import GameState
import FEN
import Bitboard
import Move
import Eval
import NNUE

initialSC :: StateComplex
initialSC = fENToComplex "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

testSC :: StateComplex
testSC = fENToComplex "rnb1kbnr/pppqpppp/8/8/8/8/PPPPPPPP/RNB1KBNR w KQkq - 0 1"


displayInCheck :: Bool -> String
displayInCheck True = "\ESC[33mYou are in check. You must get out of check\ESC[0m"
displayInCheck False = ""

play :: StateComplex -> IO ()
play sc = do
    print $ gameState sc
    putStrLn ("FEN: " ++ showFEN (gameState sc))
    putStrLn "\n---------------------------------\n"
    putStrLn $ displayInCheck $ complexInCheck sc
    putStrLn "Your next move:"
    move <- getLine
    if move == "exit"
        then return ()
        else do
            let newsc = moveAction sc $ readAN move sc
            case newsc of
                Nothing -> do
                    putStrLn "\ESC[31mIllegal move. try again \n---------------------------------------------\ESC[0m"
                    play sc
                Just a -> play a

testEval :: IO ()
testEval = do
    a <- initializeNNUE
    print a
    print $ evalBoard testSC