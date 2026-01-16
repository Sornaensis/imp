{-# LANGUAGE RecordWildCards #-}

module Imp.RuntimeSpec (spec) where

import Control.Lens (Lens', lens)
import Control.Monad.State.Strict (evalState)
import Data.Int (Int32)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Imp.Runtime

data Inner = Inner { _val :: Int }

data Env = Env
  { _count :: Int
  , _inner :: Inner
  }


countL :: Lens' Env Int
countL = lens _count (\s a -> s { _count = a })

innerL :: Lens' Env Inner
innerL = lens _inner (\s a -> s { _inner = a })

valL :: Lens' Inner Int
valL = lens _val (\s a -> s { _val = a })


spec :: Spec
spec = describe "Imp.Runtime Ref helpers" $ do
  it "gets and sets via composed refs" $ do
    let env0 = Env 1 (Inner 2)
        refCount = Ref countL
        valRef = Ref innerL .^ valL
        result = flip evalState env0 $ do
          c <- getRef refCount
          setRef refCount (c + 5)
          v <- getRef valRef
          modRef valRef (+ 3)
          pure (c, v)
    result `shouldBe` (1, 2)

  it "modifies nested value" $ do
    let env0 = Env 0 (Inner 10)
        valRef = Ref innerL .^ valL
        env1 = flip evalState env0 $ do
          modRef valRef (+ 7)
          getRef valRef
    env1 `shouldBe` 17

  prop "setRef/getRef round-trips" $ \initial newVal ->
    let env0 = Env initial (Inner 0)
        refCount = Ref countL
        result = flip evalState env0 $ do
          setRef refCount newVal
          getRef refCount
    in result == newVal

  prop "impAdd matches (+) for Int32" $ \a b ->
    impAdd (a :: Int32) b == a + b

