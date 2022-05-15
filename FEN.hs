module FEN
    where
import GameState

split :: Char -> String -> [String]
split c str = case break (==c) str of
    (ls,[]) -> [ls]
    (ls,x:xs) -> ls : split c xs -- this function splits. break splits a list into two at a given condition

parserRank :: String -> [Maybe Piece]
parserRank = concatMap $ readWithNum . readFENPiece
    where readWithNum (Left piece) = [Just piece]
          readWithNum (Right num) = replicate num Nothing

showFENBoard :: Board -> String
showFENBoard = concatMap (foldr compressFEN "/") . reverse
    where
        compressFEN Nothing (x:xs)
            | x `elem` ['1'..'9'] = succ x : xs
            | otherwise = '1':x:xs
        compressFEN (Just p) x = show p ++ x
        compressFEN _ _ = error"Board parse error"

readActiveColor :: String -> Color
readActiveColor "b" = Black
readActiveColor "w" = White

showActiveColor :: Color -> String
showActiveColor Black = "b"
showActiveColor White = "w"

readCastlingRights :: String -> Castlable -> Castlable
readCastlingRights str = case str of
    "-" -> id
    "" -> id
    ('K':rs) -> readCastlingRights rs . (\x -> x {whiteKingSide = True})
    ('Q':rs) -> readCastlingRights rs . (\x -> x {whiteQueenSide = True})
    ('k':rs) -> readCastlingRights rs . (\x -> x {blackKingSide = True})
    ('q':rs) -> readCastlingRights rs . (\x -> x {blackQueenSide = True})
    _ -> id
-- Currying to make recursion easier.

showCastlingRights :: Bool -> Castlable -> String -- Bool is to detect whether it is called recursively or not
showCastlingRights True (Castlable False False False False) = "-"
showCastlingRights False (Castlable False False False False) = ""
showCastlingRights _ c@(Castlable True _ _ _)= "K" ++ showCastlingRights False c{whiteKingSide = False}
showCastlingRights _ c@(Castlable _ True _ _)= "Q" ++ showCastlingRights False c{whiteQueenSide = False}
showCastlingRights _ c@(Castlable _ _ True _)= "k" ++ showCastlingRights False c{blackKingSide = False}
showCastlingRights _ c@(Castlable _ _ _ True)= "q"

readEnPassantSquare :: String -> Maybe Coordinate
readEnPassantSquare "-" = Nothing
readEnPassantSquare str = Just $ Coordinate (head str, read [str !! 1] :: Int)

showEnPassantSquare :: Maybe Coordinate -> String
showEnPassantSquare Nothing = "-"
showEnPassantSquare (Just (Coordinate c)) = fst c : show (snd c)

parserFEN :: String -> GameState
parserFEN str =
    let list = words str in
        let boardList = split '/' $ head list in
            GameState (reverse $ map parserRank boardList) (readActiveColor $ list !! 1)
                      (readCastlingRights (list !! 2) $ Castlable False False False False)
                      (readEnPassantSquare $ list !! 3)
                      (read (list !! 4) :: Int)
                      (read (list !! 5) :: Int)

showFEN :: GameState -> String
showFEN s =
    init (showFENBoard (board s)) ++ " " ++
    showActiveColor (activeColor s) ++ " " ++
    showCastlingRights True (castlable s) ++ " " ++
    showEnPassantSquare (enPassantSquare s) ++ " " ++
    show (halfMoveClock s) ++ " " ++
    show (fullMoveNumber s)