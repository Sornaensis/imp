module Main (main) where

import Test.Hspec
import qualified Imp.ASTSpec as ASTSpec
import qualified Imp.ParserSpec as ParserSpec
import qualified Imp.RuntimeSpec as RuntimeSpec
import qualified Imp.CodegenSpec as CodegenSpec
import qualified Imp.ValidateSpec as ValidateSpec
import qualified Imp.DSLSpec as DSLSpec
import qualified UserSpec

main :: IO ()
main = hspec $ do
  ASTSpec.spec
  ParserSpec.spec
  RuntimeSpec.spec
  CodegenSpec.spec
  ValidateSpec.spec
  DSLSpec.spec
  UserSpec.spec
