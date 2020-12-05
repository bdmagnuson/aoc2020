{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as TIO
import Control.Applicative
import Control.Lens

data Height = In Int | Cm Int | Nada Int deriving (Show)

data Passport = Passport
  { _byr :: Maybe Int
  , _iyr :: Maybe Int
  , _eyr :: Maybe Int
  , _hgt :: Maybe Height
  , _hcl :: Maybe Text
  , _ecl :: Maybe Text
  , _pid :: Maybe Text
  , _cid :: Maybe Int
  } deriving (Show)

blank = Passport Nothing Nothing Nothing Nothing
                 Nothing Nothing Nothing Nothing

$(makeLenses ''Passport)

parseT = P.takeTill (\c -> c == ' ' || c == '\n')

parseInput :: T.Text -> Either String [Passport]
parseInput = P.parseOnly (P.many' parsePassport)
  where parsePassport = do
          fields <- P.many' parseField <* P.endOfLine
          return $ foldl1 (.) fields blank

        parseField = (     parseByr
                       <|> parseIyr
                       <|> parseEyr
                       <|> parseHgt
                       <|> parseHcl
                       <|> parseEcl
                       <|> parsePid
                       <|> parseCid
                     ) <* (P.space <|> P.char '\n')
        parseInt  c = P.decimal >>= (\x -> return $ \y -> y & c .~ Just x)
        parseText c = parseT    >>= (\x -> return $ \y -> y & c .~ Just x)
        parseByr = P.string "byr:" *> parseInt  byr
        parseIyr = P.string "iyr:" *> parseInt  iyr
        parseEyr = P.string "eyr:" *> parseInt  eyr

        parseHcl = P.string "hcl:" *> parseText hcl
        parseEcl = P.string "ecl:" *> parseText ecl
        parsePid = P.string "pid:" *> parseText pid
        parseCid = P.string "cid:" *> parseInt  cid
        parseHgt = do
          P.string "hgt:"
          val <- P.decimal
          parseCm val <|> parseIn val <|> parseNada val
        parseCm x = do
          P.string "cm"
          return $ (\y -> y & hgt .~ Just (Cm x))
        parseIn x = do
          P.string "in"
          return $ (\y -> y & hgt .~ Just (In x))
        parseNada x = do
          return $ (\y -> y & hgt .~ Just (Nada x))



isValid :: Passport -> Bool
isValid p
   | Passport
      (Just byr')
      (Just iyr')
      (Just eyr')
      (Just hgt')
      (Just hcl')
      (Just ecl')
      (Just pid')
      _
       <- p = validByr byr' && validIyr iyr' && validEyr eyr' && validHgt hgt' && validHcl hcl' && validEcl ecl' && validPid pid'
   | otherwise = False
  where validByr byr' = (byr' >= 1920) && (byr' <= 2002)
        validIyr iyr' = (iyr' >= 2010) && (iyr' <= 2020)
        validEyr eyr' = (eyr' >= 2020) && (eyr' <= 2030)
        validHgt hgt' = case hgt' of
                         (In x) -> (x >= 59) && (x <= 76)
                         (Cm x) -> (x >= 150) && (x <= 193)
                         (Nada _) -> False
        validHcl hcl' = (T.head hcl' == '#') &&
                        (all (\x -> elem x ("0123456789abcdef" :: String)) ((T.unpack . T.tail) hcl')) &&
                        ((T.length . T.tail) hcl' == 6)
        validEcl ecl' = ecl' `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validPid pid' = (T.length pid' == 9) && (all (\x -> elem x ("0123456789" :: String)) ((T.unpack . T.tail) pid'))



doit :: IO ()
doit = do
  input <- TIO.readFile "input4.txt"
  case parseInput input of
    Left err -> error err
    Right a -> (putStrLn . show) (length $ (filter isValid a))


