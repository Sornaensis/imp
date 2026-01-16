{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserSpec (spec) where

import Control.Lens (view)
import Control.Monad (forM)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Haskell.TH (Dec(..), Info(..), nameBase, reify)
import Language.Haskell.TH.Syntax (lift)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Imp.CodegenGolden (renderDecSummary)

import UserSpec.UserCaps (HasAuditLog(..), HasClock(..), HasSessionStore(..), HasUserOps(..), HasUserStore(..), createSession, createUser, deleteUser, requireAuditLog, requireClock, requireSessionStore, requireUserOps, requireUserStore, updatePreferences, updateProfile)
import UserSpec.UserLogic (defaultUser, sampleUser)
import qualified UserSpec.UserModel as UserModel
import UserSpec.UserRuntime

newtype TestM a = TestM { runTestM :: State TestEnv a }
  deriving (Functor, Applicative, Monad)

data TestEnv = TestEnv
  { teUsers :: Map UserModel.UserId UserModel.User
  , teSessions :: Map UserModel.SessionId UserModel.Session
  , teAudit :: [UserModel.AuditEvent]
  , teNow :: Int64
  }

instance HasUserStore TestM where
  cap_userStore_getUser userId = TestM $ gets (Map.lookup userId . teUsers)
  cap_userStore_putUser user = TestM $ modify $ \env ->
    env { teUsers = Map.insert (view UserModel.userId user) user (teUsers env) }
  cap_userStore_deleteUser userId = TestM $ modify $ \env ->
    env { teUsers = Map.delete userId (teUsers env) }
  cap_userStore_listUsers = TestM $ gets (Map.elems . teUsers)

instance HasSessionStore TestM where
  cap_sessionStore_getSession sessionId = TestM $ gets (Map.lookup sessionId . teSessions)
  cap_sessionStore_putSession session = TestM $ modify $ \env ->
    env { teSessions = Map.insert (view UserModel.sessionId session) session (teSessions env) }
  cap_sessionStore_deleteSession sessionId = TestM $ modify $ \env ->
    env { teSessions = Map.delete sessionId (teSessions env) }
  cap_sessionStore_listSessions = TestM $ gets (Map.elems . teSessions)

instance HasAuditLog TestM where
  cap_auditLog_appendEvent event = TestM $ modify $ \env ->
    env { teAudit = teAudit env <> [event] }

instance HasClock TestM where
  cap_clock_now = TestM $ gets teNow

runTest :: TestEnv -> TestM a -> (a, TestEnv)
runTest env action = runState (runTestM action) env

userCapsSummary :: String
userCapsSummary = $(do
  let classNames =
        [ ''HasUserStore
        , ''HasSessionStore
        , ''HasAuditLog
        , ''HasClock
        ]
      requireNames =
        [ 'requireUserStore
        , 'requireSessionStore
        , 'requireAuditLog
        , 'requireClock
        ]
  classDecs <- fmap concat $ forM classNames $ \name -> do
    info <- reify name
    case info of
      ClassI dec _ -> pure [dec]
      TyConI dec -> pure [dec]
      _ -> fail ("Expected class declaration for " <> nameBase name)
  sigDecs <- forM requireNames $ \name -> do
    info <- reify name
    case info of
      VarI _ ty _ -> pure (SigD name ty)
      _ -> fail ("Expected value declaration for " <> nameBase name)
  let decs = classDecs <> sigDecs
      order =
        [ "HasUserStore"
        , "requireUserStore"
        , "HasSessionStore"
        , "requireSessionStore"
        , "HasAuditLog"
        , "requireAuditLog"
        , "HasClock"
        , "requireClock"
        ]
      nameOf d = case d of
        ClassD _ name _ _ _ -> nameBase name
        SigD name _ -> nameBase name
        _ -> ""
      pick n = find (\d -> nameOf d == n) decs
  lift (renderDecSummary (mapMaybe pick order))
  )

userOpsHasSupers :: Bool
userOpsHasSupers = $(do
  info <- reify ''HasUserOps
  case info of
    ClassI (ClassD preds _ _ _ _) _ -> lift (length preds == 3)
    _ -> fail "Expected HasUserOps class"
  )

spec :: Spec
spec = describe "User platform end-to-end" $ do
  it "creates a user and appends audit event" $ do
    let env0 = TestEnv Map.empty Map.empty [] 42
        userId = UserModel.UserId 1
        email = UserModel.Email "demo@example.com"
        (_, env1) = runTest env0 (createUser userId email)
    Map.member userId (teUsers env1) `shouldBe` True
    length (teAudit env1) `shouldBe` 1

  it "updates a profile display name" $ do
    let userId = UserModel.UserId 1
        email = UserModel.Email "demo@example.com"
        user0 = defaultUser userId email
        env0 = TestEnv (Map.singleton userId user0) Map.empty [] 0
        (_, env1) = runTest env0 (updateProfile userId "Ada")
        user1 = teUsers env1 Map.! userId
        display = view UserModel.displayName (view UserModel.profile user1)
    display `shouldBe` Just "Ada"

  it "creates a session and logs audit" $ do
    let env0 = TestEnv Map.empty Map.empty [] 99
        userId = UserModel.UserId 2
        (session, env1) = runTest env0 (createSession userId)
    Map.member (view UserModel.sessionId session) (teSessions env1) `shouldBe` True
    length (teAudit env1) `shouldBe` 1

  it "deletes a user and records audit" $ do
    let userId = UserModel.UserId 3
        email = UserModel.Email "demo@example.com"
        user0 = defaultUser userId email
        env0 = TestEnv (Map.singleton userId user0) Map.empty [] 0
        (_, env1) = runTest env0 (deleteUser userId)
    Map.member userId (teUsers env1) `shouldBe` False
    length (teAudit env1) `shouldBe` 1

  it "updates preferences with enum channel" $ do
    let userId = UserModel.UserId 4
        email = UserModel.Email "demo@example.com"
        user0 = defaultUser userId email
        env0 = TestEnv (Map.singleton userId user0) Map.empty [] 0
        (_, env1) = runTest env0 (updatePreferences userId UserModel.EmailChannel)
        user1 = teUsers env1 Map.! userId
    view UserModel.theme (view UserModel.prefs user1) `shouldBe` "dark"

  it "builds a sample user from tagged literals" $ do
    let user1 = sampleUser
        userId = view UserModel.userId user1
        emailValue = view UserModel.emailValue (view UserModel.email (view UserModel.profile user1))
    userId `shouldBe` UserModel.UserId 1
    emailValue `shouldBe` "demo@example.com"

  it "applies runtime block updates" $ do
    let userId = UserModel.UserId 5
        email = UserModel.Email "demo@example.com"
        user0 = defaultUser userId email
        user1 = updateDisplayName "Grace" user0
    view UserModel.displayName (view UserModel.profile user1) `shouldBe` Just "Grace"

  prop "updates nested display name" $ \name ->
    let userId = UserModel.UserId 7
        email = UserModel.Email "demo@example.com"
        user0 = defaultUser userId email
        user1 = updateDisplayName (T.pack name) user0
    in view UserModel.displayName (view UserModel.profile user1) == Just (T.pack name)

  it "bumps session timestamp via ref index" $ do
    let userId = UserModel.UserId 6
        email = UserModel.Email "demo@example.com"
        session0 = UserModel.Session (UserModel.SessionId 1) userId 10
        user0 = (defaultUser userId email) { UserModel._UserSessions = [session0] }
        user1 = bumpFirstSession user0
        firstAt = view UserModel.createdAt (head (view UserModel.sessions user1))
    firstAt `shouldBe` 11

  it "matches codegen golden for capability module" $ do
    golden <- readFile "test/golden/user-caps-module.txt"
    userCapsSummary `shouldBe` golden

  it "emits superclass constraints for UserOps" $ do
    userOpsHasSupers `shouldBe` True
