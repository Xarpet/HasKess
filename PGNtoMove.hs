to :: String -> [String]
to str =
    dropEvery3 $ words str
    where
        dropEvery3 (x:y:z:w) = y:z:dropEvery3 w
        dropEvery3 x = x