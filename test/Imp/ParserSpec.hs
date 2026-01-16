module Imp.ParserSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)

import Imp.AST
import Imp.Parser

spec :: Spec
spec = describe "Imp.Parser" $ do
  it "parses simple addition expression" $ do
    parseExpr "<test>" "1 + 2" `shouldSatisfy` \case
      Right (L _ (EBinOp OpAdd _ _)) -> True
      _ -> False

  it "parses capability call expression" $ do
    parseExpr "<test>" "capabilities.Logging.log(1)" `shouldSatisfy` \case
      Right (L _ (ECapCall "Logging" "log" [_])) -> True
      _ -> False

  it "parses tagged literals" $ do
    parseExpr "<test>" "uuid#\"abc\"" `shouldSatisfy` \case
      Right (L _ (ELit (LTagged "uuid" (LText "abc")))) -> True
      _ -> False

  it "parses capability header in program" $ do
    parseProgram "<test>" (T.pack "capabilities(Logging); var x = 1;") `shouldSatisfy` \case
      Right (Program (Just (L _ (CapHeader caps))) _) -> caps == ["Logging"]
      _ -> False

  it "parses multiple capability headers" $ do
    parseProgram "<test>" (T.pack "capabilities(Logging, Storage); var x = 1;") `shouldSatisfy` \case
      Right (Program (Just (L _ (CapHeader caps))) _) -> caps == ["Logging", "Storage"]
      _ -> False

  it "parses a simple statement" $ do
    parseProgram "<test>" (T.pack "var x: int = 3;") `shouldSatisfy` \case
      Right (Program _ [L _ (SVar "x" _ _)]) -> True
      _ -> False

  it "parses a minimal module with a type" $ do
    let src :: Text
        src = "type User { id: int; }"
    parseModule "<test>" src `shouldSatisfy` \case
      Right (Module [L _ (TType (TypeDecl "User" _ _ _))]) -> True
      _ -> False

  it "respects multiplication precedence" $ do
    parseExpr "<test>" "1 + 2 * 3" `shouldSatisfy` \case
      Right (L _ (EBinOp OpAdd _ (L _ (EBinOp OpMul _ _)))) -> True
      _ -> False

  it "parses an operator override block" $ do
    let src :: Text
        src = "operators { + = Prelude.add; }"
    parseModule "<test>" src `shouldSatisfy` \case
      Right (Module [L _ (TOperators (OpBlock [(OpAdd, ["Prelude", "add"])]))]) -> True
      _ -> False

  it "parses balanced hs blocks" $ do
    parseExpr "<test>" "hs{ {1} }" `shouldSatisfy` \case
      Right (L _ (EHs code)) -> "{1}" `T.isInfixOf` code
      _ -> False

  it "parses balanced hsM blocks" $ do
    parseExpr "<test>" "hsM{ {pure ()} }" `shouldSatisfy` \case
      Right (L _ (EHsM code)) -> "{pure ()}" `T.isInfixOf` code
      _ -> False

  it "renders parse errors with a caret" $ do
    let src = "var x = ;"
    case parseProgram "<test>" (T.pack src) of
      Left err -> errorBundlePretty err `shouldContain` "^"
      Right _ -> expectationFailure "expected parse failure"

  it "propagates spans across postfix chains" $ do
    parseExpr "<test>" "foo.bar[1]" `shouldSatisfy` \case
      Right (L (Span start end) _) ->
        unPos (sourceLine start) == 1
          && unPos (sourceColumn start) == 1
          && unPos (sourceLine end) == 1
          && unPos (sourceColumn end) == 11
      _ -> False

  it "parses a full mixed module" $ do
    let src :: Text
        src = T.unlines
          [ "newtype UserId = int;"
          , "enum Status { Active, Deleted }"
          , "type User { id: UserId; }"
          , "capability Store { get(id: UserId): Option<User>; }"
          , "operators { + = add; }"
          , "fn addOne(x: int): int { return x + 1; }"
          , "proc touch(): Unit requires (Store) { return; }"
          ]
    parseModule "<test>" src `shouldSatisfy` \case
      Right (Module items) -> length items == 7
      _ -> False

  it "parses record update expressions" $ do
    parseExpr "<test>" "user { age: 1, name: \"x\" }" `shouldSatisfy` \case
      Right (L _ (ERecordUpdate _ updates)) -> length updates == 2
      _ -> False
