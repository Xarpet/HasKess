module Bitboard where

import GameState
import FEN
import Data.Word
import Data.Bits
import Data.List

-- definitions and fetch functions

type Bitboard = Word64

data StateComplex = StateComplex GameState BitboardState

gameStateFromComplex :: StateComplex -> GameState
gameStateFromComplex (StateComplex gs _) = gs
bitboardStateFromComplex :: StateComplex -> BitboardState
bitboardStateFromComplex (StateComplex _ bs) = bs

data Direction = NW | N | NE | W | E | SW | S | SE |
                NWW | NNW | NNE | NEE | SWW | SSW | SSE | SEE

splitn :: Int -> [a] -> [[a]] -- splits a list
splitn n = unfoldr (\s -> if null s then Nothing else Just $ splitAt n s)

emptyBitboard :: Bitboard
emptyBitboard = 0

data BitboardState = BitboardState {
    whitePawn :: Bitboard,
    whiteKnight :: Bitboard,
    whiteBishop :: Bitboard,
    whiteRook :: Bitboard,
    whiteQueen :: Bitboard,
    whiteKing :: Bitboard,
    blackPawn :: Bitboard,
    blackKnight :: Bitboard,
    blackBishop :: Bitboard,
    blackRook :: Bitboard,
    blackQueen :: Bitboard,
    blackKing :: Bitboard,
    whiteTotal :: Bitboard,
    blackTotal :: Bitboard,
    total :: Bitboard
} deriving (Eq)
-- we go a1, b1 ... h8.

pieceInBitboardState :: Piece -> (BitboardState -> Bitboard)
pieceInBitboardState (Piece Pawn White) = whitePawn
pieceInBitboardState (Piece Pawn Black) = blackPawn
pieceInBitboardState (Piece Knight White) = whiteKnight
pieceInBitboardState (Piece Knight Black) = blackKnight
pieceInBitboardState (Piece Bishop White) = whiteBishop
pieceInBitboardState (Piece Bishop Black) = blackBishop
pieceInBitboardState (Piece Rook White) = whiteRook
pieceInBitboardState (Piece Rook Black) = blackRook
pieceInBitboardState (Piece Queen White) = whiteQueen
pieceInBitboardState (Piece Queen Black) = blackQueen
pieceInBitboardState (Piece King White) = whiteKing
pieceInBitboardState (Piece King Black) = blackKing

colorInBitboardState :: Color -> (BitboardState -> Bitboard)
colorInBitboardState White = whiteTotal
colorInBitboardState Black = blackTotal

-- Bitboard Operations:

complementBitboard :: Bitboard -> Bitboard
complementBitboard = complement

intersectBitboard :: Bitboard -> Bitboard -> Bitboard
intersectBitboard = (.&.) -- bitwise and

unionBitboard :: Bitboard -> Bitboard -> Bitboard
unionBitboard = (.|.) -- bitwise or

xorBitboard :: Bitboard -> Bitboard -> Bitboard
xorBitboard = xor

totalValue :: Color -> BitboardState -> Int -- total value based on standard piece count
totalValue White board = popCount (whitePawn board) +
                         3 * popCount (whiteBishop board) +
                         3 * popCount (whiteKnight board) +
                         5 * popCount (whiteRook board) +
                         9 * popCount (whiteQueen board)
totalValue Black board = popCount (blackPawn board) +
                         3 * popCount (blackBishop board) +
                         3 * popCount (blackKnight board) +
                         5 * popCount (blackRook board) +
                         9 * popCount (blackQueen board)

showBitboard :: Bitboard -> String
showBitboard b = unlines . reverse $ splitn 8 $ showBitboardCount 64 b

showBitboardCount :: Int -> Bitboard -> String
showBitboardCount 0 _ = []
showBitboardCount count b = head (show (mod b 2)) : showBitboardCount (count - 1) (div b 2)

instance Show BitboardState where
    show (BitboardState a b c d e f g h i j k l m n o) =
        "\nwhitePawn\n" ++
        showBitboard a ++ "\nwhiteKnight\n" ++
        showBitboard b ++ "\nwhiteBishop\n" ++
        showBitboard c ++ "\nwhiteRook\n" ++
        showBitboard d ++ "\nwhiteQueen\n" ++
        showBitboard e ++ "\nwhiteKing\n" ++
        showBitboard f ++ "\nblackPawn\n" ++
        showBitboard g ++ "\nblackKnight\n" ++
        showBitboard h ++ "\nblackBishop\n" ++
        showBitboard i ++ "\nblackRook\n" ++
        showBitboard j ++ "\nblackQueen\n" ++
        showBitboard k ++ "\nblackKing\n" ++
        showBitboard l ++ "\nwhiteTotal\n" ++
        showBitboard m ++ "\nblackTotal\n" ++
        showBitboard n ++ "\ntotal\n" ++
        showBitboard o -- I'm sure there's a better way

-- Board <-> Bitboard

boardToBitboard :: Board -> BitboardState
boardToBitboard board = BitboardState
    {
        whitePawn = whitePawn,
        whiteKnight = whiteKnight,
        whiteBishop = whiteBishop,
        whiteRook = whiteRook,
        whiteQueen = whiteQueen,
        whiteKing = whiteKing,
        blackPawn = blackPawn,
        blackKnight = blackKnight,
        blackBishop = blackBishop,
        blackRook = blackRook,
        blackQueen = blackQueen,
        blackKing = blackKing,
        whiteTotal = whiteTotal,
        blackTotal = blackTotal,
        total = total
    }
    where
        whitePawn = pieceToBitboard (Piece Pawn White) board
        whiteKnight = pieceToBitboard (Piece Knight White) board
        whiteBishop = pieceToBitboard (Piece Bishop White) board
        whiteRook = pieceToBitboard (Piece Rook White) board
        whiteQueen = pieceToBitboard (Piece Queen White) board
        whiteKing = pieceToBitboard (Piece King White) board
        blackPawn = pieceToBitboard (Piece Pawn Black) board
        blackKnight = pieceToBitboard (Piece Knight Black) board
        blackBishop = pieceToBitboard (Piece Bishop Black) board
        blackRook = pieceToBitboard (Piece Rook Black) board
        blackQueen = pieceToBitboard (Piece Queen Black) board
        blackKing = pieceToBitboard (Piece King Black) board
        whiteTotal = foldr unionBitboard zeroBits [whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen, whiteKing]
        blackTotal = foldr unionBitboard zeroBits [blackPawn, blackKnight, blackBishop, blackRook, blackQueen, blackKing]
        total = unionBitboard whiteTotal blackTotal

        pieceToBitboard :: Piece -> Board -> Bitboard
        pieceToBitboard piece board = foldl (ifEQPieceThenFlipBit piece) zeroBits $ zip [0..63] $ concat board

        ifEQPieceThenFlipBit :: Piece -> Bitboard -> (Int, Maybe Piece) -> Bitboard
        ifEQPieceThenFlipBit piece bitboard (index, maybePiece)
            | maybePiece == Just piece = bitboard `setBit` index
            | otherwise = bitboard

bitboardToBoard :: BitboardState -> Board
bitboardToBoard b = splitn 8 $ map (testSquareFromBitboard b) [0..63] where

    testSquareFromBitboard :: BitboardState -> Int -> Maybe Piece
    testSquareFromBitboard b index
        | testBit (whitePawn b) index = Just (Piece Pawn White)
        | testBit (whiteKnight b) index = Just (Piece Knight White)
        | testBit (whiteBishop b) index = Just (Piece Bishop White)
        | testBit (whiteRook b) index = Just (Piece Rook White)
        | testBit (whiteQueen b) index = Just (Piece Queen White)
        | testBit (whiteKing b) index = Just (Piece King White)
        | testBit (blackPawn b) index = Just (Piece Pawn Black)
        | testBit (blackKnight b) index = Just (Piece Knight Black)
        | testBit (blackBishop b) index = Just (Piece Bishop Black)
        | testBit (blackRook b) index = Just (Piece Rook Black)
        | testBit (blackQueen b) index = Just (Piece Queen Black)
        | testBit (blackKing b) index = Just (Piece King Black)
        | otherwise = Nothing

coordinateToBit :: Coordinate -> Int
coordinateToBit (Coordinate (char,int)) = (fromEnum char - 97) * 8 + int - 1 -- note bit index start from 0

bitToCoordinate :: Int -> Coordinate
bitToCoordinate x = Coordinate (toEnum $ div x 8 + 97, mod x 8 + 1)

fENToComplex :: String -> StateComplex
fENToComplex fen = StateComplex gs (boardToBitboard $ board gs) where
    gs = parserFEN fen

-- movement

pieceSlide :: Piece -> Bool -- True means sliding, false means one-move only
pieceSlide (Piece Pawn White) = False
pieceSlide (Piece Pawn Black) = False
pieceSlide (Piece Knight White) = False
pieceSlide (Piece Knight Black) = False
pieceSlide (Piece Bishop White) = True
pieceSlide (Piece Bishop Black) = True
pieceSlide (Piece Rook White) = True
pieceSlide (Piece Rook Black) = True
pieceSlide (Piece Queen White) = True
pieceSlide (Piece Queen Black) = True
pieceSlide (Piece King White) = False
pieceSlide (Piece King Black) = False

pieceAttackDirection :: Piece -> [Direction]
pieceAttackDirection (Piece Pawn White) = [NW, NE]
pieceAttackDirection (Piece Pawn Black) = [SW, SE]
pieceAttackDirection (Piece Knight White) = [NWW, NNW, NNE, NEE, SWW, SSW, SSE, SEE]
pieceAttackDirection (Piece Knight Black) = [NWW, NNW, NNE, NEE, SWW, SSW, SSE, SEE]
pieceAttackDirection (Piece Bishop White) = [NW,NE,SW,SE]
pieceAttackDirection (Piece Bishop Black) = [NW,NE,SW,SE]
pieceAttackDirection (Piece Rook White) = [W,N,E,S]
pieceAttackDirection (Piece Rook Black) = [W,N,E,S]
pieceAttackDirection (Piece Queen White) = [W,N,E,S,NW,NE,SW,SE]
pieceAttackDirection (Piece Queen Black) = [W,N,E,S,NW,NE,SW,SE]
pieceAttackDirection (Piece King White) = [W,N,E,S,NW,NE,SW,SE]
pieceAttackDirection (Piece King Black) = [W,N,E,S,NW,NE,SW,SE]

pieceMoveDirection :: Piece -> [Direction]
pieceMoveDirection (Piece Pawn White) = [N]
pieceMoveDirection (Piece Pawn Black) = [S]
pieceMoveDirection (Piece Knight White) = [NWW, NNW, NNE, NEE, SWW, SSW, SSE, SEE]
pieceMoveDirection (Piece Knight Black) = [NWW, NNW, NNE, NEE, SWW, SSW, SSE, SEE]
pieceMoveDirection (Piece Bishop White) = [NW,NE,SW,SE]
pieceMoveDirection (Piece Bishop Black) = [NW,NE,SW,SE]
pieceMoveDirection (Piece Rook White) = [W,N,E,S]
pieceMoveDirection (Piece Rook Black) = [W,N,E,S]
pieceMoveDirection (Piece Queen White) = [W,N,E,S,NW,NE,SW,SE]
pieceMoveDirection (Piece Queen Black) = [W,N,E,S,NW,NE,SW,SE]
pieceMoveDirection (Piece King White) = [W,N,E,S,NW,NE,SW,SE]
pieceMoveDirection (Piece King Black) = [W,N,E,S,NW,NE,SW,SE]

directionOffset :: Direction -> Int
directionOffset NW = 7
directionOffset N = 8
directionOffset NE = 9
directionOffset W = -1
directionOffset E = 1
directionOffset SW = -9
directionOffset S = -8
directionOffset SE = -7
directionOffset NWW = 6
directionOffset NNW = 15
directionOffset NNE = 17
directionOffset NEE = 10
directionOffset SWW = -10
directionOffset SSW = -17
directionOffset SSE = -15
directionOffset SEE = -6

directionMask :: Direction -> Bitboard -- there are only five kinds of mask. (A, AB, None, H, GH)
directionMask NW = 18374403900871474942
directionMask N = 18446744073709551615 -- There is no need to mask since out of bounds are discarded anyways
directionMask NE = 9187201950435737471
directionMask W = 18374403900871474942
directionMask E = 9187201950435737471
directionMask SW = 18374403900871474942
directionMask S = 18446744073709551615
directionMask SE = 9187201950435737471
directionMask NWW = 18229723555195321596
directionMask NNW = 18374403900871474942
directionMask NNE = 9187201950435737471
directionMask NEE = 4557430888798830399
directionMask SWW = 18229723555195321596
directionMask SSW = 18374403900871474942
directionMask SSE = 9187201950435737471
directionMask SEE = 4557430888798830399

offsetSquares :: Bitboard -> Int -> Bitboard -> Bitboard
offsetSquares b offset mask =
    shift (mask `intersectBitboard` b) offset

offsetSquaresSlide :: Bitboard -> Bitboard -> Int -> Bitboard -> Bitboard
offsetSquaresSlide b total offset mask
    | b == 0 = emptyBitboard
    | otherwise =
        oneStep `unionBitboard`
        offsetSquaresSlide (oneStep `intersectBitboard` complement total) total offset mask where
            oneStep = offsetSquares b offset mask
-- do one offset, and do the offset of result excluding the occupied squares


pieceAttackSquares :: Piece -> BitboardState -> Bitboard
pieceAttackSquares piece@(Piece kind color) bstate
    | not $ pieceSlide piece = foldl unionBitboard emptyBitboard $ allDirections (offsetSquares b)
    | otherwise = foldl unionBitboard emptyBitboard $ allDirections (offsetSquaresSlide b tot)
    where
        allDirections zipFunction = zipWith zipFunction
         (map directionOffset $ pieceAttackDirection piece) (map directionMask $ pieceAttackDirection piece)
        b = pieceInBitboardState piece bstate
        ally = colorInBitboardState color bstate
        enemy = colorInBitboardState (opponent color) bstate
        tot = total bstate

showAllAttackSquares :: BitboardState -> String
showAllAttackSquares bs =
    "\nWhite Pawn\n" ++ showBitboard (pieceAttackSquares (Piece Pawn White) bs) ++
    "\nBlack Pawn\n" ++ showBitboard (pieceAttackSquares (Piece Pawn Black) bs) ++
    "\nWhite Knight\n" ++ showBitboard (pieceAttackSquares (Piece Knight White) bs) ++
    "\nBlack Knight\n" ++ showBitboard (pieceAttackSquares (Piece Knight Black) bs) ++
    "\nWhite Bishop\n" ++ showBitboard (pieceAttackSquares (Piece Bishop White) bs) ++
    "\nBlack Bishop\n" ++ showBitboard (pieceAttackSquares (Piece Bishop Black) bs) ++
    "\nWhite Rook\n" ++ showBitboard (pieceAttackSquares (Piece Rook White) bs) ++
    "\nBlack Rook\n" ++ showBitboard (pieceAttackSquares (Piece Rook Black) bs) ++
    "\nWhite Queen\n" ++ showBitboard (pieceAttackSquares (Piece Queen White) bs) ++
    "\nBlack Queen\n" ++ showBitboard (pieceAttackSquares (Piece Queen Black) bs) ++
    "\nWhite King\n" ++ showBitboard (pieceAttackSquares (Piece King White) bs) ++
    "\nBlack King\n" ++ showBitboard (pieceAttackSquares (Piece King Black) bs)