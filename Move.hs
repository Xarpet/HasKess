module Move where

import GameState
import FEN
import Bitboard

import Data.Bits
import Data.Word

data MoveType = Relocate | Promote | Castle | EnPassant

data Move = Move {
    moveType :: MoveType,
    mover :: Piece, -- this is also what you promote to when you refer to a pawn promoting
    from :: Int, -- this is in bit
    to :: Int
}

-- predicates

inCheck :: BitboardState -> Color -> Bool
inCheck bs White =
    emptyBitboard /= intersectBitboard (whiteKing bs) (
    pieceAttackSquares (Piece Pawn Black) bs `unionBitboard`
    pieceAttackSquares (Piece Knight Black) bs `unionBitboard`
    pieceAttackSquares (Piece Bishop Black) bs `unionBitboard`
    pieceAttackSquares (Piece Rook Black) bs `unionBitboard`
    pieceAttackSquares (Piece Queen Black) bs `unionBitboard`
    pieceAttackSquares (Piece King Black) bs)
inCheck bs Black =
    emptyBitboard /= intersectBitboard (blackKing bs) (
    pieceAttackSquares (Piece Pawn White) bs `unionBitboard`
    pieceAttackSquares (Piece Knight White) bs `unionBitboard`
    pieceAttackSquares (Piece Bishop White) bs `unionBitboard`
    pieceAttackSquares (Piece Rook White) bs `unionBitboard`
    pieceAttackSquares (Piece Queen White) bs `unionBitboard`
    pieceAttackSquares (Piece King White) bs)

squareInCheck :: BitboardState -> Color -> Int -> Bool
squareInCheck bs White b =
    inCheck bs{whiteKing = bit b} White
squareInCheck bs Black b =
    inCheck bs{blackKing = bit b} Black


moveLegality :: Move -> StateComplex -> Bool
moveLegality move@(Move Relocate mover@(Piece Pawn color) from to) (StateComplex gs bs)
    | testBit (colorInBitboardState (opponent color) bs) to = -- a capture
        Just mover == testSquareFromBitboard bs from &&
        testBit (singlePieceAttackSquares mover bs from) to &&
        not (inCheck (moveBitboard bs move) color)
    | otherwise =
        Just mover == testSquareFromBitboard bs from &&
        testBit (singlePieceMoveSquares mover bs from) to &&
        not (testBit (total bs) to) && -- the to square must be empty
        not (inCheck (moveBitboard bs move) color)
-- pawn is special because move and attack are different

moveLegality move@(Move Relocate mover@(Piece _ color) from to) (StateComplex gs bs) =
    Just mover == testSquareFromBitboard bs from && -- mover exists
    testBit (singlePieceMoveSquares mover bs from) to && -- mover can move to the to square
    not (inCheck (moveBitboard bs move) color) -- this move is legal

moveLegality (Move Promote (Piece King color) _ _) _ = False
moveLegality (Move Promote (Piece Pawn color) _ _) _ = False -- you cannot promote to pawn or king

moveLegality move@(Move Promote mover@(Piece kind color) from to) (StateComplex gs bs)
    | testBit (colorInBitboardState (opponent color) bs) to =
        Just (Piece Pawn color) == testSquareFromBitboard bs from &&
        testBit (singlePieceAttackSquares (Piece Pawn color) bs from) to &&
        not (inCheck (moveBitboard bs move) color) &&
        promoteSquares to color -- check if to square can promote
    | otherwise =
        Just (Piece Pawn color) == testSquareFromBitboard bs from &&
        testBit (singlePieceMoveSquares (Piece Pawn color) bs from) to &&
        not (inCheck (moveBitboard bs move) color) &&
        not (testBit (total bs) to) && 
        promoteSquares to color 
        where
            promoteSquares to White = testBit (18374686479671623680 :: Bitboard) to -- test if to is at 8th rank
            promoteSquares to Black = testBit (255 :: Bitboard) to -- test if to is at 1th rank 
moveLegality move@(Move Castle mover@(Piece King color) from to) (StateComplex gs bs) =
    Just mover == testSquareFromBitboard bs from &&
    not (inCheck bs color) && -- you cannot castle in check
    not (inCheck (moveBitboard bs move) color) &&
    not (squareInCheck bs color $ div (from + to) 2) && -- the passed square mustn't be in check
    not (testBit (total bs) to) && -- the to square mustn't be occupied
    not (testBit (total bs) $ div (from + to) 2) && -- the passed square mustn't be occupied
    queenSideEmpty from to && -- test if the queenside b file square is empty
    canCastle (castlable gs) color to where -- checks the squares and castlability
        canCastle c White 2 = whiteQueenSide c
        canCastle c White 6 = whiteKingSide c
        canCastle c Black 58 = blackQueenSide c
        canCastle c Black 62 = blackKingSide c
        canCastle _ _ _ = False
        queenSideEmpty from to
            | from < to = True
            | otherwise = not (testBit (total bs) (to - 1))


moveLegality (Move Castle (Piece _ _) _ _) _ = False

moveLegality move@(Move EnPassant mover@(Piece Pawn color) from to) (StateComplex gs bs) =
    maybe (-1) coordinateToBit (enPassantSquare gs) == to && -- to square must be the en passant square
    Just mover == testSquareFromBitboard bs from &&
    testBit (singlePieceAttackSquares mover bs from) to &&
    not (inCheck (moveBitboard bs move) color)

moveLegality (Move EnPassant (Piece _ _) _ _) _ = False

-- move -> board

--Note that these two moves are UNCHECKED. The checking will be left to movecomplex
moveBitboard :: BitboardState -> Move -> BitboardState
moveBitboard bs (Move Relocate piece from to)
    | capture == Nothing = moved --when isn't a capture, just move the piece
    | otherwise = updatePieceInBitboardState moved (fromJust capture) $ clearBit (pieceInBitboardState (fromJust capture) bs) to where -- when there is capture, you must clear the captured bit
        capture = testSquareFromBitboard bs to -- the piece that is captured
        moved = updatePieceInBitboardState bs piece $ setBit (clearBit (pieceInBitboardState piece bs) from) to
        fromJust (Just a) = a

moveBitboard bs (Move Promote piece@(Piece _ color) from to)
    | capture == Nothing = promoted --when isn't a capture, just move the piece
    | otherwise = updatePieceInBitboardState promoted (fromJust capture) $ clearBit (pieceInBitboardState (fromJust capture) bs) to where
        capture = testSquareFromBitboard bs to -- the piece that is captured
        promoted = updatePieceInBitboardState moved piece $ setBit (pieceInBitboardState piece bs) to -- set promoted piece
        moved = updatePieceInBitboardState bs (Piece Pawn color) $ clearBit (pieceInBitboardState (Piece Pawn color) bs) from -- delete the pawn
        fromJust (Just a) = a

moveBitboard bs (Move Castle king@(Piece _ color) from to)
    | from > to = -- queenside castle
        updatePieceInBitboardState kingMoved (Piece Rook color) $ setBit (clearBit (pieceInBitboardState (Piece Rook color) bs) $ queenRookBit color) (div (from + to) 2)
    | otherwise = --kingside castle
        updatePieceInBitboardState kingMoved (Piece Rook color) $ setBit (clearBit (pieceInBitboardState (Piece Rook color) bs) $ kingRookBit color) (div (from + to) 2) where
            kingRookBit White = 7
            kingRookBit Black = 63
            queenRookBit White = 0
            queenRookBit Black = 56
            kingMoved = updatePieceInBitboardState bs king $ setBit (clearBit (pieceInBitboardState king bs) from) to

moveBitboard bs (Move EnPassant pawn@(Piece _ color) from to) =
    updatePieceInBitboardState moved (Piece Pawn $ opponent color) $ clearBit (pieceInBitboardState (Piece Pawn $ opponent color) bs) $ capturedCord color where
        moved = updatePieceInBitboardState bs pawn $ setBit (clearBit (pieceInBitboardState pawn bs) from) to
        capturedCord White = to - 8 -- calculate the cord of taken pawn
        capturedCord Black = to + 8

moveGameState :: GameState -> Move -> GameState
moveGameState = undefined