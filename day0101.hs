-- This uses lazy IO, so it might run out of memory on large inputs.
-- So far I can't do Haskell IO efficiently. Or Haskell IO in general.

main :: IO ()
main = do putStrLn "Enter/paste lines with masses, then a newline:"
          input' <- lines <$> getContents
          let input = takeWhile (not . null) input'
          let masses = map read (input) :: [Integer]

          let sum = foldl (+) 0 (map fuel masses)

          putStr "Total fuel required: "
          print sum

fuel :: Integer -> Integer
fuel m = div m 3 - 2
