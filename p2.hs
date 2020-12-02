
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as TIO

data Entry = Entry
  { min' :: Int
  , max' :: Int
  , letter :: Char
  , password :: Text
  } deriving (Show)

parseInput :: T.Text -> Either String [Entry]
parseInput = P.parseOnly (P.many' parseLine)
  where
    parseLine = do
      (min, max) <- parseRange
      letter <- P.anyChar <* P.char ':' <* P.space
      password <- P.takeWhile (not . P.isEndOfLine) <* P.endOfLine
      return $ Entry min max letter password
    parseRange = do
      min <- P.decimal
      P.char '-'
      max <- P.decimal
      P.space
      return (min, max)

isValid :: Entry -> Bool
isValid e = (cnt >= (min' e)) && (cnt <= (max' e))
  where cnt = fromIntegral $ (T.length . T.filter (== (letter e))) (password e)

isValid2 :: Entry -> Bool
isValid2 e = match min' /= match max'
  where match m = T.index (password e) (m e - 1) == letter e

part1 = length . filter (== True) . map isValid
part2 = length . filter (== True) . map isValid2

doit :: IO ()
doit = do
  input <- TIO.readFile "input2.txt"
  case parseInput input of
     Left err -> error err
     Right a -> do
       putStrLn (show (part1 a))
       putStrLn (show (part2 a))




