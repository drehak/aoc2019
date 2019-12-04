-- Maybe there's needlessly too many types and functions. Still, this works.

import Data.Maybe

main :: IO ()
main = do putStrLn "Enter the first wire's path:"
          input1 <- getLine
          let wire1 = dirsToLines (P 0 0) (map strToDirection (split ',' input1))
          putStrLn "Enter the second wire's path:"
          input2 <- getLine
          let wire2 = dirsToLines (P 0 0) (map strToDirection (split ',' input2))

          let nearest = minimum (map (manhattan (P 0 0)) (intersects wire1 wire2))

          putStr "Distance to nearest intersection: "
          print nearest

-- Probably not very robust, but valid inputs should be fine.
split :: Char -> String -> [String]
split _ "" = []
split c s = h : split c (drop (length h + 1) s) where h = takeWhile (/=c) s

data Direction = L Integer | R Integer | U Integer | D Integer
instance Show Direction where
    show (L i) = 'L' : ' ' : (show i)
    show (R i) = 'R' : ' ' : (show i)
    show (U i) = 'U' : ' ' : (show i)
    show (D i) = 'D' : ' ' : (show i)

strToDirection :: String -> Direction
strToDirection s = case head s of
                      'L' -> L (read (tail s) :: Integer)
                      'R' -> R (read (tail s) :: Integer)
                      'U' -> U (read (tail s) :: Integer)
                      'D' -> D (read (tail s) :: Integer)

data Point = P Integer Integer  -- x y
instance Show Point where show (P x y) = "P (" ++ show x ++ " " ++ show y ++ ")"

manhattan :: Point -> Point -> Integer
manhattan (P x1 y1) (P x2 y2) = abs (x1 - x2) + abs (y1 - y2) 

movePoint :: Point -> Direction -> Point
movePoint (P x y) (L dx) = P (x - dx) y
movePoint (P x y) (R dx) = P (x + dx) y
movePoint (P x y) (U dy) = P x (y + dy)
movePoint (P x y) (D dy) = P x (y - dy)

data Line = H Integer Integer Integer  -- horizontal line (x1 .. x2) y
          | V Integer Integer Integer  -- vertical   line x (y1 .. y2)
instance Show Line where
    show (H x1 x2 y) = "H (" ++ show x1 ++ " .. " ++ show x2 ++ ") " ++ show y
    show (V x y1 y2) = "V " ++ show x ++ " (" ++ show y1 ++ " .. " ++ show y2 ++ ")"

makeLine :: Point -> Direction -> Line
makeLine (P x y) (L dx) = H (x - dx) x y
makeLine (P x y) (R dx) = H x (x + dx) y
makeLine (P x y) (U dy) = V x y (y + dy)
makeLine (P x y) (D dy) = V x (y - dy) y

dirsToLines :: Point -> [Direction] -> [Line]
dirsToLines _ [] = []
dirsToLines p (d:ds) = makeLine p d : dirsToLines (movePoint p d) ds

intersect :: Line -> Line -> (Maybe Point)
intersect (H _ _ _) (H _ _ _) = Nothing
intersect (V _ _ _) (V _ _ _) = Nothing
intersect (H x1 x2 y) (V x y1 y2) = if x1 < x && x < x2 && y1 < y && y < y2 then Just (P x y) else Nothing
intersect (V x y1 y2) (H x1 x2 y) = if x1 < x && x < x2 && y1 < y && y < y2 then Just (P x y) else Nothing

intersects :: [Line] -> [Line] -> [Point]
intersects w1 w2 = catMaybes [ int | l1 <- w1, l2 <- w2, let int = intersect l1 l2 ]
