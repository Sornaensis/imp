module Imp.ASTSpec (spec) where

import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

import Imp.AST

spec :: Spec
spec = describe "Imp.AST" $ do
  it "merges spans from start to end" $ do
    let s1 = initialPos "a"
        s2 = initialPos "b"
        s3 = initialPos "c"
        span1 = Span s1 s2
        span2 = Span s2 s3
    mergeSpan span1 span2 `shouldBe` Span s1 s3
