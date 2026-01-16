module Imp.ValidateSpec (spec) where

import qualified Data.Text as T
import Language.Haskell.TH.Syntax (lift)
import Test.Hspec

import Imp.Parser
import Imp.Validate

programDiagnosticsRender :: String
programDiagnosticsRender = $(do
  let src = "capabilities(Logging, Logging); var x = 1;"
      input = T.pack src
  case parseProgram "<test>" input of
    Left err -> fail (show err)
    Right prog -> do
      diags <- validateProgram input prog
      lift (renderDiagnostics "<test>" input diags)
  )

unknownTypeMessages :: [String]
unknownTypeMessages = $(do
  let src = "type User { id: Missing; }"
      input = T.pack src
  case parseModule "<test>" input of
    Left err -> fail (show err)
    Right mod' -> do
      diags <- validateModule input mod'
      lift (map (T.unpack . diagMessage) diags)
  )

awaitMessages :: [String]
awaitMessages = $(do
  let src = "fn foo(): unit { await hsM{pure ()}; }"
      input = T.pack src
  case parseModule "<test>" input of
    Left err -> fail (show err)
    Right mod' -> do
      diags <- validateModule input mod'
      lift (map (T.unpack . diagMessage) diags)
  )

unknownCapMessages :: [String]
unknownCapMessages = $(do
  let src = "proc foo(): unit { await capabilities.Missing.ping(); }"
      input = T.pack src
  case parseModule "<test>" input of
    Left err -> fail (show err)
    Right mod' -> do
      diags <- validateModule input mod'
      lift (map (T.unpack . diagMessage) diags)
  )

badOpMessages :: [String]
badOpMessages = $(do
  let src = "operators { + = Missing.func; }"
      input = T.pack src
  case parseModule "<test>" input of
    Left err -> fail (show err)
    Right mod' -> do
      diags <- validateModule input mod'
      lift (map (T.unpack . diagMessage) diags)
  )

spec :: Spec
spec = describe "Imp.Validate" $ do
  it "renders diagnostics with carets" $ do
    programDiagnosticsRender `shouldContain` "error:"
    programDiagnosticsRender `shouldContain` "^"

  it "flags unknown types in modules" $ do
    unknownTypeMessages `shouldSatisfy` any (T.isInfixOf "Unknown type name" . T.pack)

  it "flags await in fn bodies" $ do
    awaitMessages `shouldSatisfy` any (T.isInfixOf "await is not allowed" . T.pack)

  it "flags unknown capabilities in module bodies" $ do
    unknownCapMessages `shouldSatisfy` any (T.isInfixOf "Unknown capability" . T.pack)

  it "flags invalid operator overrides" $ do
    badOpMessages `shouldSatisfy` any (T.isInfixOf "Unknown operator mapping target" . T.pack)
