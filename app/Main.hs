{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Imp.DSL

-- Example: Define types and capabilities using impModule

[impModule|
  // Define a simple User type
  type User deriving (Show, Eq) {
    id: int;
    name: Text;
    age: int;
    active: bool;
  }
  
  // Define a capability for logging
  capability Logging requires (MonadIO) {
    info(msg: Text): Void;
    warn(msg: Text): Void;
  }
  
  // Define a capability for database operations
  capability Database {
    getUser(userId: int): User;
    saveUser(user: User): Void;
  }
|]

-- Implement the capabilities for a concrete monad

data AppEnv = AppEnv
  { envLogPrefix :: Text
  , envUsers :: [(Int32, User)]
  }

newtype AppM a = AppM { unAppM :: StateT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState AppEnv)

instance HasLogging AppM where
  cap_logging_info msg = do
    liftIO $ putStrLn $ "[INFO] " <> T.unpack msg
  
  cap_logging_warn msg = do
    liftIO $ putStrLn $ "[WARN] " <> T.unpack msg

instance HasDatabase AppM where
  cap_database_getUser userId = do
    env <- get
    case lookup userId (envUsers env) of
      Just user -> pure user
      Nothing -> pure (User userId "Unknown" 0 False)
  
  cap_database_saveUser user = do
    env <- get
    let uid = case user of User i _ _ _ -> i
        users' = (uid, user) : filter ((/= uid) . fst) (envUsers env)
    put env { envUsers = users' }

-- Example imperative procedures

-- A simple procedure that uses capabilities
updateUserAge :: (HasLogging m, HasDatabase m) => Int32 -> Int32 -> m ()
updateUserAge uid newAge = [imp|
  capabilities(Logging, Database);
  
  capabilities.logging.info("Fetching user");
  var user = await capabilities.database.getUser(uid);
  
  capabilities.logging.info("Updating age");
  var updated = user { age: newAge };
  await capabilities.database.saveUser(updated);
  capabilities.logging.info("User saved");
|]

-- Main function demonstrating the DSL
main :: IO ()
main = do
  putStrLn "=== Imperative DSL Example ==="
  putStrLn ""
  
  let initialEnv = AppEnv
        { envLogPrefix = "APP"
    , envUsers = [(1, User 1 "Alice" 30 True)]
        }
  
  -- Run example procedure
  evalStateT (unAppM (updateUserAge 1 31)) initialEnv
  

