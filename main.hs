import GameState
import FEN
import Bitboard
import Move



bs :: StateComplex
bs = fENToComplex "rnbqkb1r/ppppppPp/8/8/8/8/PPPPPP1P/RNBQKBNR w KQkq - 0 1"

move :: Move
move = (Move )

main :: IO ()
main = do
    print $ gameState bs
    print $ gameState $ moveComplex bs move