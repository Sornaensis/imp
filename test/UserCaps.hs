module UserCaps
  ( HasUserStore(..)
  , HasSessionStore(..)
  , HasAuditLog(..)
  , HasClock(..)
  , HasUserOps(..)
  , requireUserStore
  , requireSessionStore
  , requireAuditLog
  , requireClock
  , requireUserOps
  , createUser
  , updateProfile
  , createSession
  , deleteUser
  , updatePreferences
  ) where

import Data.Maybe (fromJust, isJust)

import Imp.DSL
import UserLogic (defaultUser, renameProfile)
import UserModel (AuditEvent(..), Email(..), NotificationChannel(..), Preferences(..), Session(..), SessionId(..), User(..), UserId(..), prefs, profile, sessions, status, userId)
import qualified UserModel

[impModule|
  capability UserStore {
    getUser(id: UserId): Option<User>;
    putUser(user: User): Unit;
    deleteUser(id: UserId): Unit;
    listUsers(): List<User>;
  }

  capability SessionStore {
    getSession(id: SessionId): Option<Session>;
    putSession(session: Session): Unit;
    deleteSession(id: SessionId): Unit;
    listSessions(): List<Session>;
  }

  capability AuditLog {
    appendEvent(event: AuditEvent): Unit;
  }

  capability Clock {
    now(): long;
  }

  capability UserOps requires (UserStore, AuditLog, Clock) {
    createUser(user: User): Unit;
  }

  proc createUser(uid: UserId, emailVal: Email): Unit requires (UserStore, AuditLog, Clock) {
    var user = defaultUser(uid, emailVal);
    await capabilities.UserStore.putUser(user);
    var now = await capabilities.Clock.now();
    await capabilities.AuditLog.appendEvent(new AuditEvent("create_user", hs{ Just uid }, now, "created"));
  }

  proc updateProfile(uid: UserId, name: text): Unit requires (UserStore, AuditLog) {
    var existing = await capabilities.UserStore.getUser(uid);
    if (hs{ isJust existing }) {
      var user = hs{ fromJust existing };
      var updated = new User(user.userId, user.status, renameProfile(user.profile, name), user.prefs, user.sessions);
      await capabilities.UserStore.putUser(updated);
      await capabilities.AuditLog.appendEvent(new AuditEvent("update_profile", hs{ Just uid }, 0, "updated"));
    }
  }

  proc createSession(uid: UserId): Session requires (SessionStore, AuditLog, Clock) {
    var now = await capabilities.Clock.now();
    var sessionId = new SessionId(sessionId#1);
    var session = new Session(sessionId, uid, now);
    await capabilities.SessionStore.putSession(session);
    await capabilities.AuditLog.appendEvent(new AuditEvent("create_session", hs{ Just uid }, now, "created"));
    return session;
  }

  proc deleteUser(uid: UserId): Unit requires (UserStore, SessionStore, AuditLog) {
    await capabilities.UserStore.deleteUser(uid);
    await capabilities.SessionStore.deleteSession(new SessionId(sessionId#1));
    await capabilities.AuditLog.appendEvent(new AuditEvent("delete_user", hs{ Just uid }, 0, "deleted"));
  }

  proc updatePreferences(uid: UserId, channel: NotificationChannel): Unit requires (UserStore, AuditLog) {
    var existing = await capabilities.UserStore.getUser(uid);
    if (hs{ isJust existing }) {
      var user = hs{ fromJust existing };
      var updated = new User(user.userId, user.status, user.profile, new Preferences("dark", hs{ [channel] }), user.sessions);
      await capabilities.UserStore.putUser(updated);
      await capabilities.AuditLog.appendEvent(new AuditEvent("update_prefs", hs{ Just uid }, 0, "updated"));
    }
  }
|]
