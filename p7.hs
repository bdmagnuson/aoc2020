{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.List

input = unsafeReadFileT "input7.txt"

createDef :: Text-> BagDef
createDef i =
  case parseInput input of
    Left err -> error err
    Right a -> a

def = createDef input

type Bag = T.Text
type BagContents = [(Int, Bag)]

type BagDef =  (M.HashMap Bag BagContents)

data BagTree = BagTree Bag [BagTree] deriving (Show)

foo :: BagDef -> Bag -> BagContents
foo d b = concat $ unfoldr f [b]
  where f :: [Bag] -> Maybe (BagContents, [Bag])
        f b = let cont = concat $ catMaybes (map ((flip M.lookup) d) b)
              in if null cont
                 then Nothing
                 else Just (cont, map snd cont)

part1 =
  let has = map (elem "shiny gold" . map snd . foo def) (M.keys def)
   in sum (map (\x -> if x then 1 else 0) has)

part2 = embed def "shiny gold" - 1
  where
    embed g b =
      1 + (sum $ map f (fromJust $ M.lookup b g))
      where f (x, y) = x * (embed g y)

parseInput :: Text -> Either String BagDef
parseInput = P.parseOnly parseBags
  where
    parseId = P.takeWhile (\c -> isAlpha c)
    parseBags = do
      bags <- P.many' parseBag
      return $ M.fromList bags
    parseName = do
      modifier <- parseId
      P.space
      color <- parseId
      return (modifier <> " " <> color)
    parseBag = do
      name <- parseName
      P.string " bags contain "
      contents <- noBags <|> (P.many' parseContent)
      P.endOfLine
      return $ (name, contents)
    parseContent = do
      count <- P.decimal
      P.space
      name <- parseName
      P.space
      (P.try $ P.string "bags") <|> (P.string "bag")
      (P.char ',' <* P.space) <|> (P.char '.')
      return (count, name)
    noBags = do
      P.string "no other bags."
      return []


