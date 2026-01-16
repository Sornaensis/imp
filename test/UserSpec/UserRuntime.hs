module UserSpec.UserRuntime
  ( applyDefaultPreferences
  , bumpFirstSession
  , updateDisplayName
  ) where

import Imp.DSL
import UserSpec.UserModel

[impModule|
  fn applyDefaultPreferences(user: User): User {
    return user { prefs: hs{ defaultPreferences } };
  }

  fn bumpFirstSession(user: User): User {
    var sessionsVal = user.sessions;
    var updated = hs{ case sessionsVal of
      [] -> sessionsVal
      (s:rest) -> (s { _SessionCreatedAt = _SessionCreatedAt s + 1 }) : rest
      };
    return user { sessions: hs{ updated } };
  }

  fn updateDisplayName(name: Text, user: User): User {
    return user { profile: user.profile { displayName: hs{ Just name } } };
  }
|]
