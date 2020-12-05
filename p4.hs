{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as TIO
import Control.Applicative

data Field =
    Byr  Int
  | Iyr  Int
  | Eyr  Int
  | Hgt  Text
  | Hcl  Text
  | Ecl  Text
  | Pid  Text
  | Cid  Int deriving (Show)

type Passport = [Field]

parseT = P.takeTill (\c -> c == ' ' || c == '\n')

parseInput :: T.Text -> Either String [Passport]
parseInput = P.parseOnly (P.many' parsePassport)
  where parsePassport = P.many' parseField <* P.endOfLine
        parseField = (     parseByr
                       <|> parseIyr
                       <|> parseEyr
                       <|> parseHgt
                       <|> parseHcl
                       <|> parseEcl
                       <|> parsePid
                       <|> parseCid
                     ) <* (P.space <|> P.char '\n')
        parseInt  c = P.decimal >>= (\x -> return $ c x)
        parseText c = parseT  >>= (\x -> return $ c x)
        parseByr = P.string "byr:" *> parseInt  Byr
        parseIyr = P.string "iyr:" *> parseInt  Iyr
        parseEyr = P.string "eyr:" *> parseInt  Eyr
        parseHgt = P.string "hgt:" *> parseText Hgt
        parseHcl = P.string "hcl:" *> parseText Hcl
        parseEcl = P.string "ecl:" *> parseText Ecl
        parsePid = P.string "pid:" *> parseText  Pid
        parseCid = P.string "cid:" *> parseInt  Cid

isByr (Byr _) = True
isByr _ = False
isIyr (Iyr _) = True
isIyr _ = False
isEyr (Eyr _) = True
isEyr _ = False
isHgt (Hgt _) = True
isHgt _ = False
isHcl (Hcl _) = True
isHcl _ = False
isEcl (Ecl _) = True
isEcl _ = False
isPid (Pid _) = True
isPid _ = False
isCid (Cid _) = True
isCid _ = False

anyByr :: Passport -> Bool
anyByr = any isByr
anyIyr = any isIyr
anyEyr = any isEyr
anyHgt = any isHgt
anyHcl = any isHcl
anyEcl = any isEcl
anyPid = any isPid
--anyCid = any isCid
--


isValid :: Passport -> Bool
isValid x = anyByr x && anyIyr x && anyEyr x && anyHgt x && anyHcl x && anyEcl x && anyPid x


doit :: IO ()
doit = do
  input <- TIO.readFile "input4.txt"
  case parseInput input of
    Left err -> error err
    Right a -> (putStrLn . show) (length (filter isValid a))


