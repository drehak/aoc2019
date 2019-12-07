import Data.Array

main :: IO ()
main = do putStrLn "Enter the password bounds: "
          input <- getLine
          let bounds = map read (split '-' input) :: [Int]

          if length bounds /= 2 then
              do putStrLn "Please enter two integers separated by comma. "
          else
              do let count = length [ n | n <- [head bounds .. last bounds],
                                          isAscending n, hasDoubles n ]

                 putStr "Number of possible passwords: "
                 print count

split :: Char -> String -> [String]
split _ "" = []
split c s = h : split c (drop (length h + 1) s) where h = takeWhile (/=c) s

isAscending :: Int -> Bool
isAscending i = foldl (&&) True $ zipWith (>=) (tail s) s where s = show i

splitByValue :: Eq a => [a] -> [[a]]
splitByValue [] = []
splitByValue l = h : splitByValue (drop (length h) l)
    where h = takeWhile (== (head l)) l

hasDoubles :: Int -> Bool
hasDoubles = foldl (||) False . map (>=2) . map length . splitByValue . show
