import System.IO
import Data.List.Split
import qualified Data.HashSet as S

toUnion :: [String] -> S.HashSet Char
toUnion = S.fromList . concat

toIntersection :: [String] -> S.HashSet Char
toIntersection = foldl1 S.intersection . map S.fromList

doit :: IO ()
doit = do
  input <- readFile "input6.txt"
  let groups = map lines (splitOn "\n\n" input)
  (putStrLn . show) (sum $ map (S.size . toUnion) groups)
  (putStrLn . show) (sum $ map (S.size . toIntersection) groups)
