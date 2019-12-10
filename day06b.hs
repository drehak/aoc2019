main :: IO ()
main = do putStrLn "Enter/paste lines with masses, then a newline:"
          input' <- lines <$> getContents
          let input = takeWhile (not . null) input'
          let orbitMap = map listToPair . map (split ')') $ input

          let youchain = orbitChain orbitMap "YOU"
          let sanchain = orbitChain orbitMap "SAN"
          let req = minimum [ ys | y <- [ 0 .. (length youchain) - 1 ], s <- [ 0 .. (length sanchain) - 1 ],
                              youchain !! y == sanchain !! s, let ys = y + s - 2 ]

          putStr "YOU-SAN orbital transfers required: "
          print req

split :: Char -> String -> [String]
split _ "" = []
split c s = h : split c (drop (length h + 1) s) where h = takeWhile (/=c) s

listToPair :: [a] -> (a,a)
listToPair l = (head l, last l)

orbits :: [(String, String)] -> String -> String
orbits ssl s = if found == [] then "" else fst . head $ found
    where found = [ ss | ss <- ssl, s == snd ss ]

orbitChain :: [(String, String)] -> String -> [String]
orbitChain ssl s = s : (if parent == "" then [] else orbitChain ssl parent)
    where parent = orbits ssl s
