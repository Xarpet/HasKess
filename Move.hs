{-# LANGUAGE RecordWildCards #-}
module Move where

import GameState
import FEN
import Bitboard

import Data.Bits

data MoveType = Relocate | Promote | Castle | EnPassant
    deriving (Eq, Show)

data Move = Move {
    moveType :: MoveType,
    mover :: Piece, -- this is also what you promote to when you refer to a pawn promoting
    from :: Int, -- this is in bit
    to :: Int
} deriving (Eq, Show)

blackPlusOne :: Color -> Int
blackPlusOne Black = 1
blackPlusOne White = 0

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

complexInCheck :: StateComplex -> Bool
complexInCheck (StateComplex GameState{activeColor = White} bs) =
    emptyBitboard /= intersectBitboard (whiteKing bs) (
    pieceAttackSquares (Piece Pawn Black) bs `unionBitboard`
    pieceAttackSquares (Piece Knight Black) bs `unionBitboard`
    pieceAttackSquares (Piece Bishop Black) bs `unionBitboard`
    pieceAttackSquares (Piece Rook Black) bs `unionBitboard`
    pieceAttackSquares (Piece Queen Black) bs `unionBitboard`
    pieceAttackSquares (Piece King Black) bs)
complexInCheck (StateComplex GameState{activeColor = Black} bs) =
    emptyBitboard /= intersectBitboard (blackKing bs) (
    pieceAttackSquares (Piece Pawn White) bs `unionBitboard`
    pieceAttackSquares (Piece Knight White) bs `unionBitboard`
    pieceAttackSquares (Piece Bishop White) bs `unionBitboard`
    pieceAttackSquares (Piece Rook White) bs `unionBitboard`
    pieceAttackSquares (Piece Queen White) bs `unionBitboard`
    pieceAttackSquares (Piece King White) bs)

moveLegality :: Move -> StateComplex -> Bool
moveLegality move@(Move Relocate mover@(Piece Pawn color) from to) (StateComplex gs bs)
    | testBit (colorInBitboardState (opponent color) bs) to = -- a capture
        color == activeColor gs && -- right side to move
        Just mover == testSquareFromBitboard bs from && --mover exists
        testBit (singlePieceAttackSquares mover bs from) to && -- mover can attack
        not (inCheck (moveBitboard bs move) color)
    | otherwise =
        color == activeColor gs &&
        Just mover == testSquareFromBitboard bs from &&
        testBit (singlePieceMoveSquares mover bs from) to &&
        not (testBit (total bs) to) && -- the to square must be empty
        not (inCheck (moveBitboard bs move) color)
-- pawn is special because move and attack are different

moveLegality move@(Move Relocate mover@(Piece _ color) from to) (StateComplex gs bs) =
    color == activeColor gs &&
    Just mover == testSquareFromBitboard bs from && -- mover exists
    testBit (singlePieceMoveSquares mover bs from) to && -- mover can move to the to square
    not (inCheck (moveBitboard bs move) color) -- this move is legal

moveLegality (Move Promote (Piece King color) _ _) _ = False
moveLegality (Move Promote (Piece Pawn color) _ _) _ = False -- you cannot promote to pawn or king

moveLegality move@(Move Promote mover@(Piece kind color) from to) (StateComplex gs bs)
    | testBit (colorInBitboardState (opponent color) bs) to =
        color == activeColor gs &&
        Just (Piece Pawn color) == testSquareFromBitboard bs from &&
        testBit (singlePieceAttackSquares (Piece Pawn color) bs from) to &&
        not (inCheck (moveBitboard bs move) color) &&
        promoteSquares to color -- check if to square can promote
    | otherwise =
        color == activeColor gs &&
        Just (Piece Pawn color) == testSquareFromBitboard bs from &&
        testBit (singlePieceMoveSquares (Piece Pawn color) bs from) to &&
        not (inCheck (moveBitboard bs move) color) &&
        not (testBit (total bs) to) &&
        promoteSquares to color
        where
            promoteSquares to White = testBit (18374686479671623680 :: Bitboard) to -- test if to is at 8th rank
            promoteSquares to Black = testBit (255 :: Bitboard) to -- test if to is at 1th rank 
moveLegality move@(Move Castle mover@(Piece King color) from to) (StateComplex gs bs) =
    color == activeColor gs &&
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
    color == activeColor gs &&
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




moveComplex :: StateComplex -> Move -> StateComplex -- note here we don't check legality of move
moveComplex (StateComplex GameState{..} bs)  move@(Move Relocate (Piece Pawn color) from to)
    | capture /= Nothing = StateComplex { -- if there is capture
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Nothing, -- capture is ofc nothing
            halfMoveClock = 0, -- pawn moves are not counted as fifty move
            fullMoveNumber = fullMoveNumber + blackPlusOne color -- only black's move increment
        },
        bitboardState = newBitboard
    }
    | from - to == 8 || from - to == (-8) = StateComplex { -- if pawn one-step move
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Nothing,
            halfMoveClock = 0,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    | otherwise = StateComplex { -- if pawn two-step move
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Just $ bitToCoordinate (div (from + to) 2),
            halfMoveClock = 0,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
    },
        bitboardState = newBitboard
    }
    where
        capture = testSquareFromBitboard bs to
        newBitboard = moveBitboard bs move

moveComplex (StateComplex GameState{..} bs)  move@(Move Relocate piece@(Piece King color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = loseCastleKing color castlable,
            enPassantSquare = Nothing,
            halfMoveClock = resetHalfMove,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        resetHalfMove
            | testSquareFromBitboard bs to == Nothing = halfMoveClock + 1
            | otherwise = 0 -- if captured then reset
        newBitboard = moveBitboard bs move
        loseCastleKing White c =
            c {
                whiteKingSide = False,
                whiteQueenSide = False
            }
        loseCastleKing Black c =
            c {
                blackKingSide = False,
                blackQueenSide = False
            }

moveComplex (StateComplex GameState{..} bs)  move@(Move Relocate piece@(Piece Rook color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = loseCastleRook color from castlable,
            enPassantSquare = Nothing,
            halfMoveClock = resetHalfMove,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        resetHalfMove
            | testSquareFromBitboard bs to == Nothing = halfMoveClock + 1
            | otherwise = 0 -- if captured then reset
        newBitboard = moveBitboard bs move
        loseCastleRook White from c@Castlable{..}
            | whiteKingSide && from == 7 =
                c{
                    whiteKingSide = False
                }
            | whiteQueenSide && from == 0 =
                c{
                    whiteQueenSide = False
                }
            | otherwise = c
        loseCastleRook Black from c@Castlable{..}
            | blackKingSide && from == 63 =
                c{
                    blackKingSide = False
                }
            | blackQueenSide && from == 56 =
                c{
                    blackQueenSide = False
                }
            | otherwise = c

moveComplex (StateComplex GameState{..} bs)  move@(Move Relocate piece@(Piece kind color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Nothing,
            halfMoveClock = resetHalfMove,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        resetHalfMove
            | testSquareFromBitboard bs to == Nothing = halfMoveClock + 1
            | otherwise = 0
        newBitboard = moveBitboard bs move

moveComplex (StateComplex GameState{..} bs)  move@(Move Promote piece@(Piece kind color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Nothing,
            halfMoveClock = 0,
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        newBitboard = moveBitboard bs move

moveComplex (StateComplex GameState{..} bs)  move@(Move Castle piece@(Piece kind color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = resetCastle color castlable, -- this player cannot castle anymore
            enPassantSquare = Nothing,
            halfMoveClock = halfMoveClock + 1, -- no capture or pawn move
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        resetCastle White c = c{
            whiteKingSide = False,
            whiteQueenSide = False
        }
        resetCastle Black c = c{
            blackKingSide = False,
            blackQueenSide = False
        }
        newBitboard = moveBitboard bs move

moveComplex (StateComplex GameState{..} bs)  move@(Move EnPassant piece@(Piece kind color) from to) =
    StateComplex {
        gameState = GameState {
            board = bitboardToBoard $ moveBitboard bs move,
            activeColor = opponent activeColor,
            castlable = castlable,
            enPassantSquare = Nothing, -- obviously cannot enpassant
            halfMoveClock = 0, -- it is a capture
            fullMoveNumber = fullMoveNumber + blackPlusOne color
        },
        bitboardState = newBitboard
    }
    where
        newBitboard = moveBitboard bs move

moveAction :: StateComplex -> Move -> Maybe StateComplex -- with legal detection
moveAction sc move
    | moveLegality move sc = Just $ moveComplex sc move
    | otherwise = Nothing

readMove :: String -> Move
readMove str
    | length str == 5 =     -- Relocate move
        Move Relocate piece from to
    | str !! 5 == '=' =     -- Promotion
        Move Promote (fromLeft $ readFENPiece $ last str) from to
    | str !! 5 == '0' =
        Move Castle piece from to
    | str !! 5 == 'E' =
        Move EnPassant piece from to
    | otherwise = error "Wrong Move"
    where
        piece = fromLeft $ readFENPiece $ head str
        from = coordinateToBit $ readCoordinate $ take 2 $ tail str
        to = coordinateToBit $ readCoordinate $ take 2 $ drop 2 $ tail str
        fromLeft (Left x) = x

preParserAN :: String -> String -- omit all symbols
preParserAN str
    | last str == '+' || last str == '#' =
        preParserAN $ init str -- remove the + and #
    | otherwise = str

parserAN :: String -> StateComplex -> Move
parserAN str sc@(StateComplex GameState{..} bs)
    | str == "0-0-0" || str == "O-O-O" = -- long castle
        Move Castle (Piece King activeColor) coordOfKing (coordOfKing - 2)
    | str == "0-0" || str == "O-O" = -- short castle
        Move Castle (Piece King activeColor) coordOfKing (coordOfKing + 2)
    | last (init str) == '=' =  -- promotion
        Move Promote (charToPiece activeColor $ last str) (snd $ readPiece getRidEqual sc Promote) $ to getRidEqual
        -- Because we defined the mover as the piece we promote to.
    | threeToLast str == 'x' && testSquareFromBitboard bs (to str) == Nothing =
        uncurry (Move EnPassant) (readPiece str sc EnPassant) $ to str
        -- the condition for enPassant: capture symbol but the *to* square is empty
        -- Enpassant is in essence, no different than regular capturing/relocating
    | threeToLast str == 'x' =
        uncurry (Move Relocate) (readPiece (exceptLast3 str ++ last2 str) sc Relocate) $ to str
        -- get rid of the x
    | otherwise =
        uncurry (Move Relocate) (readPiece str sc Relocate) $ to str
    where
        coordOfKing
            | activeColor == White = 4
            | otherwise = 60
        getRidEqual = (init . init) str-- get rid of =
        to str = coordinateToBit $ readCoordinate (last2 str)
        charToPiece color 'Q' = Piece Queen color
        charToPiece color 'R' = Piece Rook color
        charToPiece color 'B' = Piece Bishop color
        charToPiece color 'N' = Piece Knight color
        last2 = reverse . take 2 . reverse
        exceptLast3 = reverse . drop 3 . reverse
        threeToLast [x,y] = ' '
        threeToLast ls = reverse ls !! 2

readPiece :: String -> StateComplex -> MoveType -> (Piece, Int)
-- returns piece and location
-- input should be like "Re7" "ed7" "Red7"
-- you need to: find the piece that could move to the square LEGALLY
-- the thought process is pretending there is an opponent piece at coord and calculating its attack squares
readPiece str@[_, _] sc@(StateComplex GameState{..} bs) Relocate = -- get rid of simple pawn moves bc pawn move and attack in different ways
    (Piece Pawn activeColor, from)
    where
        from
            | activeColor == White =
                case testSquareFromBitboard bs $ coord - 8 of
                    Just _ -> coord - 8  -- indicating double square move
                    Nothing -> coord - 16
            | otherwise =
                case testSquareFromBitboard bs $ coord + 8 of
                    Just _ -> coord + 8  -- indicating double square move
                    Nothing -> coord + 16
        coord = coordinateToBit $ readCoordinate str

readPiece str@('K':xs) sc@(StateComplex GameState{..} bs) Relocate =
    (Piece King activeColor, from)
    where
        from = head $ bitboardToFlippedIndex $ pieceInBitboardState (Piece King activeColor) bs

readPiece str sc@(StateComplex GameState{..} bs) moveType =
    (piece, from)
    where
        from = testLegal $ map makeMove $ bitboardToFlippedIndex posBitboard
        -- use a ghost opponent piece to check for possible positions

        testLegal :: [Move] -> Int
        testLegal [] = error "This"
        testLegal [Move _ _ from _] = from
        testLegal (move@(Move _ _ from _):xs)
            | moveLegality move sc = from
            | otherwise = testLegal xs

        makeMove :: Int -> Move
        makeMove from = Move moveType piece from coord -- since this function is only used for relocate

        posBitboard = -- all the possible position in bitboard
            singlePieceAttackSquares (Piece kind $ opponent color) bs coord `intersectBitboard`
            limiterMask limiter `intersectBitboard`
            pieceInBitboardState piece bs

        (piece@(Piece kind color), rest) = extractPiece str sc -- extract the piece info
        coord = coordinateToBit $ readCoordinate $ (reverse . take 2 . reverse) rest
        limiter = (reverse . drop 2 . reverse) rest -- extract the limiter and coord
        -- calculate the mask (allowed squares) for each limiter
        limiterMask :: String -> Bitboard
        limiterMask [] = 18446744073709551615
        limiterMask s@[x, y] = bit $ coordinateToBit $ readCoordinate s
        limiterMask "a" = 72340172838076673
        limiterMask "b" = 144680345676153346
        limiterMask "c" = 289360691352306692
        limiterMask "d" = 578721382704613384
        limiterMask "e" = 1157442765409226768
        limiterMask "f" = 2314885530818453536
        limiterMask "g" = 4629771061636907072
        limiterMask "h" = 9259542123273814144
        limiterMask "1" = 255
        limiterMask "2" = 65280
        limiterMask "3" = 16711680
        limiterMask "4" = 4278190080
        limiterMask "5" = 1095216660480
        limiterMask "6" = 280375465082880
        limiterMask "7" = 71776119061217280
        limiterMask "8" = 18374686479671623680
        limiterMask _ = 18446744073709551615



extractPiece :: String -> StateComplex -> (Piece, String) -- it returns the piece and extract the rest of string
extractPiece ('K':str) (StateComplex GameState{activeColor = color} _) = (Piece King color, str)
extractPiece ('Q':str) (StateComplex GameState{activeColor = color} _) = (Piece Queen color, str)
extractPiece ('R':str) (StateComplex GameState{activeColor = color} _) = (Piece Rook color, str)
extractPiece ('B':str) (StateComplex GameState{activeColor = color} _) = (Piece Bishop color, str)
extractPiece ('N':str) (StateComplex GameState{activeColor = color} _) = (Piece Knight color, str)
extractPiece str (StateComplex GameState{activeColor = color} _) = (Piece Pawn color, str)

readAN :: String -> StateComplex -> Move
readAN str sc = parserAN (preParserAN str) sc