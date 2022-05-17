import Play
import NNUE
import Eval
import MoveGeneration
import FEN
import Bitboard
import GameState
import Move


main :: IO ()
main = do
    -- print $ map showMove $ legalMoves initialSC
    _ <- initializeNNUE
    print $ evalBoard initialSC
    print $ evalBoard testSC