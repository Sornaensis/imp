module Imp.DSLSpec (spec) where

import Data.Int (Int32)
import Test.Hspec

import Imp.DSL

[impModule|
  type Counter { value: int; }
|]

simpleBlock :: Maybe ()
simpleBlock = [imp| var x = 1; return; |]

counterValue :: Int32
counterValue = view value (Counter 10)

spec :: Spec
spec = describe "Imp.DSL" $ do
  it "compiles a basic imp block" $ do
    simpleBlock `shouldBe` Just ()

  it "generates lenses via impModule" $ do
    counterValue `shouldBe` 10
