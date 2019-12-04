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

fuel1 :: Integer -> Integer
fuel1 m = if m < 9 then 0 else div m 3 - 2 

fuel :: Integer -> Integer
fuel m = if m < 9 then 0 else fuel1 m + fuel (fuel1 m)
