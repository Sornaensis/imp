module UserLogic
  ( defaultProfile
  , defaultUser
  , setDisplayName
  , renameProfile
  , sampleUser
  ) where

import Imp.DSL
import UserModel (AccountStatus(..), Email(..), Profile(..), User(..), UserId(..), address, email, phone)
import qualified UserModel

[impModule|
  fn defaultProfile(emailVal: Email): Profile {
    return new Profile(null, emailVal, null, null);
  }

  fn defaultUser(uid: UserId, emailVal: Email): User {
    var profileVal = defaultProfile(emailVal);
    var prefs = hs{ UserModel.defaultPreferences };
    return new User(uid, Active, profileVal, prefs, hs{ [] });
  }

  fn setDisplayName(profileVal: Profile, name: Text): Profile {
    return new Profile(hs{ Just name }, profileVal.email, profileVal.phone, profileVal.address);
  }

  fn renameProfile(profileVal: Profile, name: Text): Profile {
    return profileVal.setDisplayName(name);
  }

  fn sampleUser(): User {
    return defaultUser(new UserId(userId#1), new Email(email#"demo@example.com"));
  }
|]
