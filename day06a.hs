-- I'm actually ashamed for a solution this cubic :(
-- (took 8s to compute on my beefy desktop)

main :: IO ()
main = do putStrLn "Enter/paste lines with masses, then a newline:"
          input' <- lines <$> getContents
          let input = takeWhile (not . null) input'
          let orbitMap = map listToPair . map (split ')') $ input

          let sums = foldl (+) 0 . map (allOrbits orbitMap) . map snd $ orbitMap

          putStr "Total orbit count: " 
          print sums

split :: Char -> String -> [String]
split _ "" = []
split c s = h : split c (drop (length h + 1) s) where h = takeWhile (/=c) s

-- not robust and very bruh, I know
listToPair :: [a] -> (a,a)
listToPair l = (head l, last l)

orbits :: [(String, String)] -> String -> String
orbits ssl s = if found == [] then "" else fst . head $ found
    where found = [ ss | ss <- ssl, s == snd ss ]

allOrbits :: [(String, String)] -> String -> Int
allOrbits ssl s = if parent == "" then 0 else 1 + allOrbits ssl parent
    where parent = orbits ssl s
