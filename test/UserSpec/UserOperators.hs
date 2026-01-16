module UserSpec.UserOperators
  ( emailAppend
  ) where

import Data.Text (Text)
import qualified Data.Text as T

emailAppend :: Text -> Text -> Text
emailAppend = T.append
