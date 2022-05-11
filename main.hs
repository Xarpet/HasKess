import GameState
import FEN
import Bitboard

main :: IO ()
main = do
    fen <- getLine
    (putStrLn . showAllAttackSquares . bitboardStateFromComplex . fENToComplex) fen