module UserSpec.UserPrimitives
  ( Email(..)
  , emailValue
  , Phone(..)
  , phoneValue
  ) where

import Control.Lens (Lens', lens)
import Data.Text (Text)

newtype Email = Email { _EmailEmailValue :: Text }
  deriving (Eq, Show)

emailValue :: Lens' Email Text
emailValue = lens _EmailEmailValue (\s a -> s { _EmailEmailValue = a })

newtype Phone = Phone { _PhonePhoneValue :: Text }
  deriving (Eq, Show)

phoneValue :: Lens' Phone Text
phoneValue = lens _PhonePhoneValue (\s a -> s { _PhonePhoneValue = a })
