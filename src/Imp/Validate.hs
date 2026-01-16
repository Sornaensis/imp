{-# LANGUAGE OverloadedStrings #-}

-- | Semantic validation and diagnostic rendering for the DSL.
module Imp.Validate
  ( Diagnostic(..)
  , validateProgram
  , validateModule
  , renderDiagnostics
  ) where

import Control.Monad (forM)
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (Q, lookupTypeName, lookupValueName)
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)

import Imp.AST

-- | A semantic diagnostic tied to a source span.
data Diagnostic = Diagnostic
  { diagSpan :: Span
  , diagMessage :: Text
  } deriving (Show, Eq)

-- | Validate an imp block program.
validateProgram :: Text -> Program -> Q [Diagnostic]
validateProgram _ (Program capHeader stmts) = do
  let capDiags = maybe [] validateCapHeaderDuplicates capHeader
      capEnv = maybe Map.empty (headerCapEnv . lVal) capHeader
  stmtDiags <- concat <$> mapM (validateStmt Set.empty capEnv) stmts
  pure (capDiags <> stmtDiags)

-- | Validate a full module.
validateModule :: Text -> Module -> Q [Diagnostic]
validateModule _ (Module items) = do
  let typeEnv = Set.fromList
        [ name
        | L _ item <- items
        , name <- case item of
            TType td -> [tdName td]
            TNewtype ntd -> [ntdName ntd]
            TEnum ed -> [edName ed]
            _ -> []
        ]
      capEnv = Map.fromList
        [ (normalizeCap (cdName cd), Set.fromList (map (normalizeCap . cmName) (cdMethods cd)))
        | L _ (TCapability cd) <- items
        ]

  itemDiags <- concat <$> mapM (validateTopItem typeEnv capEnv) items
  opDiags <- concat <$> mapM validateOpBlock items
  pure (itemDiags <> opDiags)

renderDiagnostics :: FilePath -> Text -> [Diagnostic] -> String
renderDiagnostics filename input diags =
  T.unpack (T.intercalate "\n\n" (map (renderDiagnostic filename input) diags))

renderDiagnostic :: FilePath -> Text -> Diagnostic -> Text
renderDiagnostic filename input (Diagnostic (Span start end) msg) =
  let lineNum = unPos (sourceLine start)
      colStart = unPos (sourceColumn start)
      colEnd = unPos (sourceColumn end)
      lines' = T.lines input
      lineText = if lineNum <= length lines' then lines' !! (lineNum - 1) else ""
      caretLen = max 1 (if sourceLine start == sourceLine end then colEnd - colStart else 1)
      caretLine = T.replicate (max 0 (colStart - 1)) " " <> T.replicate caretLen "^"
      header = T.pack filename <> ":" <> T.pack (show lineNum) <> ":" <> T.pack (show colStart) <> ": error: " <> msg
      tailNote = if sourceLine start /= sourceLine end
        then "\n... span ends at line " <> T.pack (show (unPos (sourceLine end))) <> ":" <> T.pack (show (unPos (sourceColumn end)))
        else ""
  in T.intercalate "\n" [header, lineText, caretLine] <> tailNote

validateTopItem :: Set Text -> Map Text (Set Text) -> L TopItem -> Q [Diagnostic]
validateTopItem typeEnv capEnv (L span' item) = case item of
  TType td -> concat <$> mapM (validateType typeEnv (Set.fromList (tdParams td))) (map fdType (tdFields td))
  TNewtype ntd -> validateType typeEnv (Set.fromList (ntdParams ntd)) (ntdType ntd)
  TEnum ed -> concat <$> mapM (validateType typeEnv (Set.fromList (edParams ed))) (concatMap evFields (edVariants ed))
  TCapability cd -> do
    let typeVars = Set.empty
    superDiags <- concat <$> mapM (validateConstraint span' typeEnv typeVars capEnv) (cdSuper cd)
    methodDiags <- concat <$> mapM (validateCapMethod typeEnv typeVars) (cdMethods cd)
    pure (superDiags <> methodDiags)
  TFn fn -> do
    typeDiags <- validateParamsAndReturn typeEnv Set.empty (fnParams fn) (fnReturn fn)
    awaitDiags <- pure (map (\sp -> Diagnostic sp "await is not allowed in fn bodies") (collectAwaitSpans (fnBody fn)))
    stmtDiags <- concat <$> mapM (validateStmt typeEnv capEnv) (fnBody fn)
    pure (typeDiags <> awaitDiags <> stmtDiags)
  TProc proc -> do
    typeDiags <- validateParamsAndReturn typeEnv Set.empty (procParams proc) (procReturn proc)
    reqDiags <- concat <$> mapM (validateConstraint span' typeEnv Set.empty capEnv) (procRequires proc)
    stmtDiags <- concat <$> mapM (validateStmt typeEnv capEnv) (procBody proc)
    pure (typeDiags <> reqDiags <> stmtDiags)
  THsDec _ -> pure []
  TOperators _ -> pure []

validateParamsAndReturn :: Set Text -> Set Text -> [Param] -> Maybe (L Type) -> Q [Diagnostic]
validateParamsAndReturn typeEnv typeVars params retType = do
  paramDiags <- concat <$> mapM (validateType typeEnv typeVars . pType) params
  retDiags <- maybe (pure []) (validateType typeEnv typeVars) retType
  pure (paramDiags <> retDiags)

validateCapMethod :: Set Text -> Set Text -> CapMethod -> Q [Diagnostic]
validateCapMethod typeEnv typeVars method = do
  paramDiags <- concat <$> mapM (validateType typeEnv typeVars . pType) (cmParams method)
  retDiags <- validateType typeEnv typeVars (cmReturn method)
  pure (paramDiags <> retDiags)

validateType :: Set Text -> Set Text -> L Type -> Q [Diagnostic]
validateType typeEnv typeVars (L span' ty) = case ty of
  TPrim _ -> pure []
  TUser names -> do
    let nameText = T.intercalate "." names
    case names of
      [n] | n `Set.member` typeVars || n `Set.member` typeEnv -> pure []
      _ -> unknownTypeDiag span' nameText
  TOption t -> validateType typeEnv typeVars t
  TList t -> validateType typeEnv typeVars t
  TMap k v -> (++) <$> validateType typeEnv typeVars k <*> validateType typeEnv typeVars v
  TGeneric name args -> do
    baseDiags <- if name `Set.member` typeVars || name `Set.member` typeEnv
      then pure []
      else unknownTypeDiag span' name
    argDiags <- concat <$> mapM (validateType typeEnv typeVars) args
    pure (baseDiags <> argDiags)

unknownTypeDiag :: Span -> Text -> Q [Diagnostic]
unknownTypeDiag span' name = do
  found <- lookupTypeName (T.unpack name)
  pure $ if found == Nothing
    then [Diagnostic span' ("Unknown type name: " <> name)]
    else []

validateConstraint :: Span -> Set Text -> Set Text -> Map Text (Set Text) -> Constraint -> Q [Diagnostic]
validateConstraint span' typeEnv typeVars capEnv (Constraint cname args) = do
  let base = last cname
      isCap = length cname == 1 && null args && Map.member (normalizeCap base) capEnv
  argDiags <- concat <$> mapM (validateType typeEnv typeVars) args
  baseDiags <- if isCap
    then pure []
    else unknownTypeDiag span' (T.intercalate "." cname)
  pure (baseDiags <> argDiags)

validateOpBlock :: L TopItem -> Q [Diagnostic]
validateOpBlock (L span' (TOperators (OpBlock mappings))) = do
  fmap concat $ forM mappings $ \(_, names) -> do
    let raw = T.unpack (T.intercalate "." names)
    found <- lookupValueName raw
    pure $ case found of
      Nothing -> [Diagnostic span' ("Unknown operator mapping target: " <> T.intercalate "." names)]
      Just _ -> []
validateOpBlock _ = pure []

validateStmt :: Set Text -> Map Text (Set Text) -> L Stmt -> Q [Diagnostic]
validateStmt typeEnv capEnv (L span' stmt) = case stmt of
  SVar _ mType mExpr -> do
    typeDiags <- maybe (pure []) (validateType typeEnv Set.empty) mType
    exprDiags <- maybe (pure []) (validateExpr typeEnv capEnv) mExpr
    pure (typeDiags <> exprDiags)
  SRef _ path -> pure (validateRefPath span' path)
  SAssign _ _ expr -> validateExpr typeEnv capEnv expr
  SAwait _ expr -> validateExpr typeEnv capEnv expr
  SExpr expr -> validateExpr typeEnv capEnv expr
  SIf cond thenStmts elseStmts -> do
    condDiags <- validateExpr typeEnv capEnv cond
    thenDiags <- concat <$> mapM (validateStmt typeEnv capEnv) thenStmts
    elseDiags <- concat <$> mapM (validateStmt typeEnv capEnv) (maybe [] id elseStmts)
    pure (condDiags <> thenDiags <> elseDiags)
  SFor init' cond incr body -> do
    initDiags <- maybe (pure []) (validateStmt typeEnv capEnv) init'
    condDiags <- maybe (pure []) (validateExpr typeEnv capEnv) cond
    incrDiags <- maybe (pure []) (validateExpr typeEnv capEnv) incr
    bodyDiags <- concat <$> mapM (validateStmt typeEnv capEnv) body
    pure (initDiags <> condDiags <> incrDiags <> bodyDiags)
  SWhile cond body -> do
    condDiags <- validateExpr typeEnv capEnv cond
    bodyDiags <- concat <$> mapM (validateStmt typeEnv capEnv) body
    pure (condDiags <> bodyDiags)
  SSwitch expr cases def -> do
    exprDiags <- validateExpr typeEnv capEnv expr
    caseDiags <- concat <$> mapM (validateSwitchCase typeEnv capEnv) cases
    defDiags <- concat <$> mapM (validateStmt typeEnv capEnv) (maybe [] id def)
    pure (exprDiags <> caseDiags <> defDiags)
  SReturn mexpr -> maybe (pure []) (validateExpr typeEnv capEnv) mexpr
  SThrow expr -> validateExpr typeEnv capEnv expr
  STry body catches fin -> do
    bodyDiags <- concat <$> mapM (validateStmt typeEnv capEnv) body
    catchDiags <- concat <$> mapM (\(_, _, stmts) -> concat <$> mapM (validateStmt typeEnv capEnv) stmts) catches
    finDiags <- concat <$> mapM (validateStmt typeEnv capEnv) (maybe [] id fin)
    pure (bodyDiags <> catchDiags <> finDiags)
  SBlock stmts -> concat <$> mapM (validateStmt typeEnv capEnv) stmts
  SBreak -> pure []
  SContinue -> pure []

validateSwitchCase :: Set Text -> Map Text (Set Text) -> SwitchCase -> Q [Diagnostic]
validateSwitchCase typeEnv capEnv (SwitchCase expr stmts) = do
  exprDiags <- validateExpr typeEnv capEnv expr
  stmtDiags <- concat <$> mapM (validateStmt typeEnv capEnv) stmts
  pure (exprDiags <> stmtDiags)

validateExpr :: Set Text -> Map Text (Set Text) -> L Expr -> Q [Diagnostic]
validateExpr typeEnv capEnv (L span' expr) = case expr of
  EVar _ -> pure []
  EThis -> pure []
  ELit _ -> pure []
  ECall f args -> do
    fDiags <- validateExpr typeEnv capEnv f
    argDiags <- concat <$> mapM (validateExpr typeEnv capEnv) args
    pure (fDiags <> argDiags)
  EMember obj _ -> validateExpr typeEnv capEnv obj
  EIndex obj idx -> (++) <$> validateExpr typeEnv capEnv obj <*> validateExpr typeEnv capEnv idx
  EBinOp _ l r -> (++) <$> validateExpr typeEnv capEnv l <*> validateExpr typeEnv capEnv r
  EUnOp _ a -> validateExpr typeEnv capEnv a
  ECapCall cap method args -> do
    let capKey = normalizeCap cap
        methodKey = normalizeCap method
        methodSet = Map.lookup capKey capEnv
        capDiags = case methodSet of
          Nothing -> [Diagnostic span' ("Unknown capability: " <> cap)]
          Just methods
            | Set.null methods -> []
            | methodKey `Set.member` methods -> []
            | otherwise -> [Diagnostic span' ("Unknown capability method: " <> cap <> "." <> method)]
    argDiags <- concat <$> mapM (validateExpr typeEnv capEnv) args
    pure (capDiags <> argDiags)
  EHs _ -> pure []
  EHsM _ -> pure []
  ENew ty args -> do
    typeDiags <- validateType typeEnv Set.empty ty
    argDiags <- concat <$> mapM (validateExpr typeEnv capEnv) args
    pure (typeDiags <> argDiags)

validateRefPath :: Span -> RefPath -> [Diagnostic]
validateRefPath span' (RefPath root parts) =
  case (root, parts) of
    (RThis, []) -> [Diagnostic span' "ref path cannot target `this` directly"]
    _ -> []

validateCapHeaderDuplicates :: L CapHeader -> [Diagnostic]
validateCapHeaderDuplicates (L span' (CapHeader caps)) =
  case dupes of
    [] -> []
    _ -> [Diagnostic span' ("Duplicate capability entries in header: " <> T.intercalate ", " dupes)]
  where
    normalized = map normalizeCap caps
    dupes = normalized \\ nub normalized

normalizeCap :: Text -> Text
normalizeCap = T.toLower

headerCapEnv :: CapHeader -> Map Text (Set Text)
headerCapEnv (CapHeader caps) =
  Map.fromList [ (normalizeCap cap, Set.empty) | cap <- caps ]

collectAwaitSpans :: [L Stmt] -> [Span]
collectAwaitSpans = concatMap awaitSpans

awaitSpans :: L Stmt -> [Span]
awaitSpans (L span' stmt) = case stmt of
  SAwait{} -> [span']
  SIf _ t f -> collectAwaitSpans t <> maybe [] collectAwaitSpans f
  SFor init' _ _ body -> maybe [] awaitSpans init' <> collectAwaitSpans body
  SWhile _ body -> collectAwaitSpans body
  SSwitch _ cases def -> concatMap caseAwaitSpans cases <> maybe [] collectAwaitSpans def
  STry body catches fin ->
    collectAwaitSpans body
      <> concatMap (collectAwaitSpans . (\(_, _, s) -> s)) catches
      <> maybe [] collectAwaitSpans fin
  SBlock xs -> collectAwaitSpans xs
  _ -> []

caseAwaitSpans :: SwitchCase -> [Span]
caseAwaitSpans (SwitchCase _ stmts) = collectAwaitSpans stmts
