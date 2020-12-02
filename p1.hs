import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P

is2020 :: [[Integer]] -> [[Integer]]
is2020 = filter (\x -> sum x == 2020)

choose 0 _ = []
choose 1 l = map pure l
choose n y@(x:xs)
  | length y >= n = (map (x:) (choose (n - 1) xs)) ++ (choose n xs)
  | otherwise = []

part1 x = map product $ is2020 $ choose 2 x
part2 x = map product $ is2020 $ choose 3 x

doit = do
  input <- TIO.readFile "input1.txt"
  case P.parseOnly (P.many' (P.decimal <* P.endOfLine)) input of
    Left err -> putStrLn err
    Right f -> do
      putStrLn (show $ part1 f)
      putStrLn (show $ part2 f)


