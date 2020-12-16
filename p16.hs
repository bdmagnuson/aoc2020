{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import qualified Data.HashMap.Lazy as HM
import Control.Monad.State.Strict
import Control.Lens
import Control.Monad
import Data.List (delete, permutations)
import Data.Maybe (fromJust)
import Debug.Trace

type Field = Text
type Range = (Integer, Integer)

data Checker = Checker (HM.HashMap Field (Integer -> Bool))

pInput = do
  rules  <- pRules
  P.string "\nyour ticket:\n"
  ticket <- pTicket
  P.string "\nnearby tickets:\n"
  otherTickets <- P.many' pTicket
  return (rules, ticket, otherTickets)
  where
    pRules = P.many' (pRule <* P.endOfLine) >>= return . HM.fromList

    pRule = do
      rule <- P.takeWhile (\c -> c /= ':')
      P.string ": "
      (lo1, hi1) <- pRange
      P.string " or "
      (lo2, hi2) <- pRange
      return (rule, \x -> ((x >= lo1) && (x <= hi1)) || ((x >= lo2) && (x <= hi2)))
    pRange = do
      lo <- P.decimal
      P.char '-'
      hi <- P.decimal
      return (lo, hi)
    pTicket = (P.sepBy P.decimal (P.char ',')) <* P.endOfLine

(rules, ticket, otherTickets) = parseInputT "input16.txt" pInput

getInvalidFields :: [Integer] -> [Integer]
getInvalidFields x = filter f x
  where f :: Integer -> Bool
        f x' = not (or (map ($ x') (HM.elems rules)))

hasInvalidFields :: [Integer] -> Bool
hasInvalidFields x = any f x
  where f :: Integer -> Bool
        f x' = not (or (map ($ x') (HM.elems rules)))

part1 = sum $ concatMap getInvalidFields otherTickets

goodTickets = filter (not . hasInvalidFields) otherTickets

validOrder :: [[Integer]] -> [Text]
validOrder x = head $ go rules x
  where go :: HM.HashMap Text (Integer -> Bool) -> [[Integer]] -> [[Text]]
        go rs vs
          | HM.null rs = return []
          | otherwise = do
              let rs' = HM.filter (\x -> all x (map head vs)) rs
              r <- HM.keys rs'
              map (r:) (go (HM.delete r rs) (map tail vs))

main = putStrLn $ show $ validOrder goodTickets


ans = ["arrival platform","train","departure platform","duration","departure track","price","arrival station","departure date","type","wagon","seat","departure location","class","row","departure station","arrival track","departure time","route","arrival location","zone"]

fn = map fst (filter (\(_, a) -> T.isPrefixOf "departure" a) (zip [0..] ans))
part2 = product $ map (ticket !!) fn

