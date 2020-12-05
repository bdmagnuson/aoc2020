
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

part1 :: [String] -> (Int, Int) -> Int
part1 [] _ = 0
part1 s (dx, dy) = tree + rest
  where
    tree = case (s !! 0) !! 0 of
             '#' -> 1
             '.' -> 0
    rest = part1 (map (drop dx) $ drop dy s) (dx, dy)

doit :: IO ()
doit = do
  input <- TIO.readFile "input3.txt"
  let grid = map (concat . repeat . T.unpack) (T.lines input)
  (putStrLn . show) $ part1 grid (3, 1)
  (putStrLn . show) $ product [ part1 grid (1, 1)
                              , part1 grid (3, 1)
                              , part1 grid (5, 1)
                              , part1 grid (7, 1)
                              , part1 grid (1, 2)
                              ]

