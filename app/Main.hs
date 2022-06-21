import Play ( play, initialSC, testSC )
import NNUE ( initializeNNUE )
import MoveGeneration ()
import Bitboard ( fENToComplex )
import AI ( choose )
import NewMoveGen ( newLegalMovesFEN, initMagic )

main :: IO ()
main = do
    _ <- initializeNNUE
    _ <- initMagic
    play initialSC