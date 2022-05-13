import GameState
import FEN
import Bitboard
import Move

bs :: BitboardState
-- bs = bitboardState $ fENToComplex "r1b1k1nr/2p4p/pp1ppb2/1n2qp1N/2P2P2/BP1BR3/P2P2PP/4K2R w Kkq - 0 11"
bs = bitboardState $ fENToComplex "rnbqkb1r/ppppppPp/8/8/8/8/PPPPPP1P/RNBQKBNR w KQkq - 0 1"

main :: IO ()
main = do
    print $ moveLegality (Move Castle (Piece King White) 4 2) $ 
        fENToComplex "rnb1kbnr/ppp1p1pp/4q3/2PpPp2/8/BP1B1P1N/P2PQ1PP/RN2K2R w KQkq - 3 11"
    -- print $ inCheck (moveBitboard (bitboardStateFromComplex $ fENToComplex 
    --     "r1b1k1nr/2p3pp/pp1ppb2/1n2qpN1/2P2P2/BP1BR3/P2P2PP/4K2R w Kkq - 0 11") 
    --     (Move Relocate (Piece Rook White) 20 21)) White
    -- print $ moveBitboard (bitboardStateFromComplex $ fENToComplex 
    --     "r1b1k1nr/2p3pp/pp1ppb2/1n2qpN1/2P2P2/BP1BR3/P2P2PP/4K2R w Kkq - 0 11") 
    --     (Move Relocate (Piece Rook White) 20 21)