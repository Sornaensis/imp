{-# LANGUAGE StandaloneDeriving #-}

module UserSpec.UserModel
  ( UserId(..)
  , SessionId(..)
  , Email(..)
  , emailValue
  , Phone(..)
  , phoneValue
  , Address(..)
  , line1
  , line2
  , city
  , country
  , Profile(..)
  , displayName
  , email
  , phone
  , address
  , AccountStatus(..)
  , NotificationChannel(..)
  , Preferences(..)
  , theme
  , notifications
  , User(..)
  , userId
  , status
  , profile
  , prefs
  , sessions
  , UserIndex(..)
  , userMap
  , sessionList
  , Session(..)
  , sessionId
  , sessionUserId
  , createdAt
  , AuditEvent(..)
  , action
  , eventUserId
  , at
  , details
  , mergeEmails
  , defaultPreferences
  ) where

import Imp.DSL
import qualified UserSpec.UserOperators as UserOperators

[impModule|
  newtype UserId = int deriving (Eq);
  newtype SessionId = int deriving (Eq);

  type Email {
    emailValue: Text;
  }

  type Phone {
    phoneValue: Text;
  }

  type Address {
    line1: Text;
    line2: Option<Text>;
    city: Text;
    country: Text;
  }

  type Profile {
    displayName: Option<Text>;
    email: Email;
    phone: Option<Phone>;
    address: Option<Address>;
  }

  enum AccountStatus { Active, Suspended, Deleted }
  enum NotificationChannel { EmailChannel, SMSChannel, PushChannel }

  type Preferences {
    theme: Text;
    notifications: List<NotificationChannel>;
  }

  type User {
    userId: UserId;
    status: AccountStatus;
    profile: Profile;
    prefs: Preferences;
    sessions: List<Session>;
  }

  type UserIndex {
    userMap: Map<UserId, User>;
    sessionList: List<Session>;
  }

  type Session {
    sessionId: SessionId;
    sessionUserId: UserId;
    createdAt: long;
  }

  type AuditEvent {
    action: Text;
    eventUserId: Option<UserId>;
    at: long;
    details: Text;
  }

  operators {
    + = UserOperators.emailAppend;
  }

  #{
    defaultPreferences :: Preferences
    defaultPreferences = Preferences
      { _PreferencesTheme = "light"
      , _PreferencesNotifications = []
      }
  }
|]

mergeEmails :: Email -> Email -> Email
mergeEmails a b = Email (view emailValue a `UserOperators.emailAppend` view emailValue b)

deriving instance Ord UserId
deriving instance Show UserId
deriving instance Ord SessionId
deriving instance Show SessionId
