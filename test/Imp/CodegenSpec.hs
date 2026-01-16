module Imp.CodegenSpec (spec) where

import Data.Int (Int32)
import Language.Haskell.TH (Body(..), Dec(..), Exp(..), Pat(..), nameBase)
import Language.Haskell.TH.Syntax (lift, runQ)
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

import Imp.AST hiding (Type)
import Imp.Codegen
import Imp.CodegenGolden (expUsesNameBase, renderDecSummary)
import Imp.Parser


addOverride :: Int32 -> Int32 -> Int32
addOverride = (-)

moduleSummary :: String
moduleSummary = $(do
  let src = "type Counter { value: int; }\ncapability Logging { log(msg: Text): Unit; }\nfn add(x: int, y: int): int { return x + y; }"
  case parseModule "<test>" src of
    Left err -> fail (show err)
    Right mod' -> do
      decs <- compileModule mod'
      lift (renderDecSummary decs)
  )

overrideUsesAdd :: Bool
overrideUsesAdd = $(do
  let src = "operators { + = addOverride; } fn add(x: int, y: int): int { return x + y; }"
  case parseModule "<test>" src of
    Left err -> fail (show err)
    Right mod' -> do
      decs <- compileModule mod'
      let addBodies =
            [ body
            | ValD (VarP name) (NormalB body) _ <- decs
            , nameBase name == "add"
            ]
      lift (any (expUsesNameBase "addOverride") addBodies)
  )

spec :: Spec
spec = describe "Imp.Codegen" $ do
  it "compiles default add operator" $ do
    let span0 = Span (initialPos "<test>") (initialPos "<test>")
        l = L span0
        expr = EBinOp OpAdd (l (ELit (LInt 1))) (l (ELit (LInt 2)))
    exp' <- runQ (compileExpr expr)
    case exp' of
      InfixE _ (VarE name) _ -> nameBase name `shouldBe` "impAdd"
      _ -> expectationFailure "expected infix operator expression"

  it "generates capability method naming and require helper" $ do
    let span0 = Span (initialPos "<test>") (initialPos "<test>")
        l = L span0
        cap = CapDecl
          { cdName = "Logging"
          , cdSuper = []
          , cdMethods =
              [ CapMethod
                  { cmName = "log"
                  , cmParams = [Param "msg" (l (TPrim PText))]
                  , cmReturn = l (TPrim PUnit)
                  }
              ]
          }
    decs <- runQ (compileModule (Module [l (TCapability cap)]))
    let methodNames =
          [ nameBase n
          | ClassD _ _ _ _ sigs <- decs
          , SigD n _ <- sigs
          ]
        topLevelSigs = [nameBase n | SigD n _ <- decs]
    methodNames `shouldContain` ["cap_logging_log"]
    topLevelSigs `shouldContain` ["requireLogging"]

  it "emits a stable module summary (golden)" $ do
    golden <- readFile "test/golden/codegen-module.txt"
    moduleSummary `shouldBe` golden

  it "uses operator overrides in codegen" $ do
    overrideUsesAdd `shouldBe` True

  it "round-trips tagged literals through codegen" $ do
    case parseExpr "<test>" "uuid#\"abc\"" of
      Left err -> expectationFailure (show err)
      Right (L _ expr) -> do
        exp' <- runQ (compileExpr expr)
        expUsesNameBase "taggedLiteral" exp' `shouldBe` True
