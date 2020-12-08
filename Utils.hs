
module Utils (unsafeReadFileT, unsafeReadFile) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe


unsafeReadFileT :: String -> Text
unsafeReadFileT = unsafePerformIO . TIO.readFile

unsafeReadFile :: String -> String
unsafeReadFile = unsafePerformIO . readFile
