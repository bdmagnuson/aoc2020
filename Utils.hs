
{-# LANGUAGE RankNTypes #-}

module Utils (unsafeReadFileT, unsafeReadFile, parseInputT) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe
import Data.Either (fromRight)
import qualified Data.Attoparsec.Text as P

unsafeReadFileT :: String -> Text
unsafeReadFileT = unsafePerformIO . TIO.readFile

unsafeReadFile :: String -> String
unsafeReadFile = unsafePerformIO . readFile

parseInputT :: forall a. String -> P.Parser a -> a
parseInputT i p =
  case P.parseOnly p (unsafeReadFileT i) of
    Left err -> error err
    Right a -> a
