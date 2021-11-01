module Luukasa.Common where

import           Data.Int  (Int64)
import           Data.Text (Text, unpack)
import           Data.Word (Word32)

type TimestampUs = Int64
type TimerCallbackId = Word32
type ErrorMessage = Text

parseInt :: Text -> Maybe Int
parseInt x =
    let parsed = reads (unpack x) :: [(Int, String)]
    in if null parsed
        then Nothing
        else Just $ fst $ head parsed

parseDouble :: Text -> Maybe Double
parseDouble x =
    let parsed = reads (unpack x) :: [(Double, String)]
    in if null parsed
        then Nothing
        else Just $ fst $ head parsed
