import Play ( play )
import NNUE ( initializeNNUE )
import MoveGeneration ()
import Bitboard ( fENToComplex )

main :: IO ()
main = do
    _ <- initializeNNUE
    putStrLn "Input fen"
    fen <- getLine
    let sc = fENToComplex fen
    play sc
    -- fen <- getLine
    -- print $ eval $ fENToComplex fen
    -- main