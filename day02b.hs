import Data.Array

main :: IO ()
main = do putStrLn "Enter the Intcode program: "
          input <- getLine
          let program = map read (split ',' input) :: [Int]
          putStrLn "Enter the desired output: "
          input <- getLine
          let desired = read input :: Int

          -- Assuming noun and verb are under 100.
          let (n,v) = head [ (n,v) | n <- [1..100], v <- [1..100], desired == testProgram program n v ]

          putStr "100 * noun + verb: "
          print (100 * n + v)

testProgram :: [Int] -> Int -> Int -> Int
testProgram al n v = do let al' = replaceAt al 1 n
                        let al = replaceAt al' 2 v
                        head (iteration al 0)

split :: Char -> String -> [String]
split _ "" = []
split c s = h : split c (drop (length h + 1) s) where h = takeWhile (/=c) s

replaceAt :: [a] -> Int -> a -> [a]
replaceAt al i a = if 0 <= i && i < length al then take i al ++ [a] ++ drop (i+1) al else al

-- Take a program and current index, and either call the next step or, if halted, return the final state.
iteration :: [Int] -> Int -> [Int]
iteration al i = if (al!!i) == 99
                     then al
                 else if (al!!i) == 1
                     then iteration (replaceAt al (al!!(i+3)) (al!!(al!!(i+1)) + al!!(al!!(i+2)))) (i+4)
                 else if (al!!i) == 2
                     then iteration (replaceAt al (al!!(i+3)) (al!!(al!!(i+1)) * al!!(al!!(i+2)))) (i+4)
                 else iteration al (i+4)
