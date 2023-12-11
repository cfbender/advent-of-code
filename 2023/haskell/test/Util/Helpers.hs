module Util.Helpers where

import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (Text)

parseTest :: Parser i -> Text -> Maybe i
parseTest inputParser input = case parseOnly inputParser input of
  Left e -> Nothing
  Right i -> Just i
