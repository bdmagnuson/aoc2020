{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Lens
import Data.Bits

type Address = Integer
type Value = Integer
data MaskBit = One | Zero | X deriving (Show, Eq)

data Machine = Machine {
  _mask :: [MaskBit],
  _gpr  :: HM.HashMap Integer Integer
} deriving (Show)

makeLenses ''Machine

data Instr
  = Mask [MaskBit]
  | Set Address Value deriving (Show)

input = parseInputT "input14.txt" pProgram

pProgram = P.many' ((P.try pMask <|> pSet) <* P.endOfLine)
  where
    pMask = do
      P.string "mask = "
      mask <- P.many' (    (P.char '1' >> return One)
                       <|> (P.char '0' >> return Zero)
                       <|> (P.char 'X' >> return X))
      return $ Mask mask
    pSet = do
      P.string "mem["
      addr <- P.decimal
      P.string "] = "
      val <- P.decimal
      return $ Set addr val

powerset xs = filterM (const [True, False]) xs

addresses :: Integer -> State Machine [Integer]
addresses a  = do
  m <- use mask
  let xbits = map fst $ filter (isX . snd) $ zip [35,34..0] m
  return $ floating ((a .&. andMask m) .|. orMask m) xbits

  where isX X = True
        isX _ = False
        orMask m = foldl (\xs x -> (shift xs 1) + (if x == One then 1 else 0)) 0 m
        andMask m = foldl (\xs x -> (shift xs 1) + (if x == X then 0 else 1)) 0 m
        floating a x = map (a .|.) (map setBits (powerset x))
        setBits l = foldl (\xs x -> setBit xs x)  0 l

execInstr :: Instr -> State Machine ()
execInstr  = \case
  Mask m  -> mask .= m
  Set a v -> do
    a <- addresses a
    mapM_ (\x -> gpr %= HM.insert x v) a

runProgram x = execState (traverse execInstr x) (Machine [] HM.empty)

part2 = sum $ (runProgram input) ^.. gpr . traversed
