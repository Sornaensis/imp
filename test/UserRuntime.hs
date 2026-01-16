module UserRuntime
  ( applyDefaultPreferences
  , bumpFirstSession
  , updateDisplayName
  ) where

import Control.Monad.State.Strict (State, execState)
import Data.Text (Text)

import Imp.DSL
import UserModel

applyDefaultPreferences :: User -> User
applyDefaultPreferences user = execState block user
  where
    block :: State User ()
    block = [imp|
      this.prefs = hs{ defaultPreferences };
      return;
    |]

bumpFirstSession :: User -> User
bumpFirstSession user =
  case _UserSessions user of
    [] -> user
    (s:rest) -> user { _UserSessions = s { _SessionCreatedAt = _SessionCreatedAt s + 1 } : rest }

updateDisplayName :: Text -> User -> User
updateDisplayName name user = execState block user
  where
    block :: State User ()
    block = [imp|
      this.profile.displayName = hs{ Just name };
      return;
    |]
