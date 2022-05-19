import Play ( play, initialSC, testSC )
import NNUE ( initializeNNUE )
import MoveGeneration ()
import Bitboard ( fENToComplex )
import AI ( choose )

main :: IO ()
main = do
    _ <- initializeNNUE
    -- print $ choose 4 initialSC
    -- putStrLn "Input fen"
    -- fen <- getLine
    -- let sc = fENToComplex fen
    play testSC 
    -- fen <- getLine
    -- print $ eval $ fENToComplex fen
    -- main