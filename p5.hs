import System.IO
import Data.List (sort)

bsp (lo, hi) [] = hi
bsp (lo, hi) (x:xs) =
  case x of
    'F' -> bsp (lo, hi - step) xs
    'B' -> bsp (lo + step, hi) xs
    'L' -> bsp (lo, hi - step) xs
    'R' -> bsp (lo + step, hi) xs
  where
    step = (hi - lo + 1) `div` 2

locate :: String -> (Int, Int)
locate s =
  let row = bsp (0, 127) (take 7 s)
      col = bsp (0, 7)   (drop 7 s)
   in (row, col)

seatId (row, col) = row * 8 + col

mySeat [] = error "damn it"
mySeat (x:y:ys)
  | x + 1 == y = mySeat (y:ys)
  | otherwise = x + 1

doit :: IO ()
doit = do
  input <- readFile "input5.txt"
  let ids = (map seatId . map locate . lines) input
  (putStrLn . show) (maximum ids)
  (putStrLn . show) (mySeat (sort ids))

