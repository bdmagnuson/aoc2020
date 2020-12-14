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

update :: Integer -> State Machine Integer
update v = do
  m <- use mask
  return $ (orMask m) .|. ((andMask m) .&. v)
  where
    andMask m = foldl (\xs x -> (shift xs 1) + (if x == X then 1 else 0)) 0 m
    orMask m = foldl (\xs x -> (shift xs 1) + (if x == One then 1 else 0)) 0 m

execInstr :: Instr -> State Machine ()
execInstr  = \case
  Mask m  -> mask .= m
  Set a v -> do
    v' <- update v
    gpr %= HM.insert a v'

runProgram x = execState (traverse execInstr x) (Machine [] HM.empty)

part1 = sum $ (runProgram input) ^.. gpr . traversed
