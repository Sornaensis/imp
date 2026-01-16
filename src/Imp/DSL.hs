{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Main DSL module - exports quasiquoters and runtime
module Imp.DSL
  ( -- * Quasiquoters
    imp
  , impModule
    -- * Runtime re-exports
  , module Imp.Runtime
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec (errorBundlePretty)

import Imp.Codegen
import Imp.Parser
import Imp.Runtime
import Imp.Validate (renderDiagnostics, validateModule, validateProgram)

-- * Quasiquoters

-- | Quasiquoter for imperative statement blocks
-- Usage: [imp| ... |]
-- Produces: Exp (monadic code)
imp :: QuasiQuoter
imp = QuasiQuoter
  { quoteExp  = impExp
  , quotePat  = notSupported "patterns"
  , quoteType = notSupported "types"
  , quoteDec  = notSupported "declarations"
  }
  where
    notSupported context _ =
      fail ("imp quasiquoter does not support " <> context)

-- | Parse and compile an imp expression block
impExp :: String -> Q Exp
impExp input = do
  loc <- location
  let filename = loc_filename loc
      inputText = T.pack input
  
  case parseProgram filename inputText of
    Left err -> fail (errorBundlePretty err)
    Right prog -> do
      diags <- validateProgram inputText prog
      if null diags
        then compileProgram prog
        else fail (renderDiagnostics filename inputText diags)

-- | Quasiquoter for module-level declarations
-- Usage: [impModule| ... |]
-- Produces: [Dec] (top-level declarations)
impModule :: QuasiQuoter
impModule = QuasiQuoter
  { quoteExp  = notSupported "expressions"
  , quotePat  = notSupported "patterns"
  , quoteType = notSupported "types"
  , quoteDec  = impModuleDec
  }
  where
    notSupported context _ =
      fail ("impModule quasiquoter does not support " <> context)

-- | Parse and compile an impModule declaration block
impModuleDec :: String -> Q [Dec]
impModuleDec input = do
  loc <- location
  let filename = loc_filename loc
      inputText = T.pack input
  
  case parseModule filename inputText of
    Left err -> fail (errorBundlePretty err)
    Right mod' -> do
      diags <- validateModule inputText mod'
      if null diags
        then compileModule mod'
        else fail (renderDiagnostics filename inputText diags)
