-- The second part is kinda fucked up.
-- Also the problem is not very clear in a few places, e.g. if wires intersect on corners or in parallel.
-- This solution assumes intersection strictly describes two wires crossing in perpendicular.
-- It worked for my input, it might not work for yours. But who knows.

import Data.Maybe

main :: IO ()
main = do putStrLn "Enter the first wire's path:"
          input1 <- getLine
          let o = (P 0 0)
          let dirs1 = map strToDirection (split ',' input1)
          let wire1 = dirsToLines o dirs1
          putStrLn "Enter the second wire's path:"
          input2 <- getLine
          let dirs2 = map strToDirection (split ',' input2)
          let wire2 = dirsToLines o dirs2

          let inters = intersects wire1 wire2
          let shortest = minimum [ d | i <- inters,
                                       let d1 = wireDistance o i dirs1,
                                       let d2 = wireDistance o i dirs2,
                                       let d = d1 + d2 ]

          putStr "Shortest wire loop length: "
          print shortest

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

lineSpan :: Line -> Integer
lineSpan (H x1 x2 _) = x2 - x1
lineSpan (V _ y1 y2) = y2 - y1

onLine :: Line -> Point -> Bool
onLine (H x1 x2 y) (P xp yp) = y == yp && x1 <= xp && xp <= x2
onLine (V x y1 y2) (P xp yp) = x == xp && y1 <= yp && yp <= y2

makeLine :: Point -> Direction -> Line
-- The interval is always ascending when we create a line in this way.
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

-- Calculates the distance from origin point to target along a given wire.
-- This time we're going through directions since the lines aren't oriented.
wireDistance :: Point -> Point -> [Direction] -> Integer
wireDistance _ _ [] = (-1)  -- This should not happen with correct inputs.
wireDistance o t (d:ds) = if onLine l t then manhattan o t
                              -- If we don't the target on the line, we return (-1) all the way to the top.
                              else if subDistance == (-1) then (-1)
                              -- If we do find it, we simply sum the line lengths.
                              else lineSpan l + subDistance
    where l = makeLine o d
          subDistance = wireDistance (movePoint o d) t ds
