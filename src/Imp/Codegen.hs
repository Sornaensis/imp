{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Template Haskell code generation for the imperative DSL
module Imp.Codegen
  ( -- * Code generation
    compileModule
  , compileProgram
  , compileExpr
  ) where

import Control.Monad (forM, when)
import Control.Lens (Lens', lens, set, view, ix)
import Data.Functor.Identity (runIdentity)
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR, complement)
import Data.ByteString (ByteString)
import Data.Char (isUpper, toLower, toUpper)
import Data.Int (Int16, Int32, Int64)
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Data.Word (Word8)
import Language.Haskell.TH hiding (Type, Stmt, Lit)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax ()
import qualified Language.Haskell.Meta.Parse as Meta
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)

import Imp.AST
import Imp.Runtime (Ref(..), setRef, modRef, (.^), impAdd, impIndex)

-- * Module compilation (impModule quasiquoter)

-- | Compile a module to Template Haskell declarations
compileModule :: Module -> Q [Dec]
compileModule (Module items) = do
  let opEnv = foldl' applyOpBlock defaultOpEnv
        [ ob | L _ (TOperators ob) <- items ]
      capEnv = Set.fromList [ cdName cd | L _ (TCapability cd) <- items ]
      typeEnv = Set.fromList
        [ name
        | L _ item <- items
        , name <- case item of
            TType td -> [tdName td]
            TNewtype ntd -> [ntdName ntd]
            TEnum ed -> [edName ed]
            _ -> []
        ]
  concat <$> mapM (compileTopItem opEnv capEnv typeEnv . lVal) items

-- | Compile a top-level item
compileTopItem :: OpEnv -> Set Text -> Set Text -> TopItem -> Q [Dec]
compileTopItem opEnv capEnv typeEnv item = case item of
  TType td -> compileTypeDecl typeEnv td
  TNewtype ntd -> compileNewtypeDecl typeEnv ntd
  TEnum ed -> compileEnumDecl typeEnv ed
  TCapability cd -> compileCapDecl capEnv typeEnv cd
  TOperators _ -> pure [] -- Operator mappings handled at expression level
  TFn fn -> compileFnDecl opEnv typeEnv fn
  TProc proc -> compileProcDecl opEnv capEnv typeEnv proc
  THsDec hsCode -> parseHaskellDecs hsCode

-- ** Type declarations

-- | Compile a type declaration to data + lenses
compileTypeDecl :: Set Text -> TypeDecl -> Q [Dec]
compileTypeDecl typeEnv (TypeDecl name params fields deriving') = do
  let typeName = mkName (T.unpack name)
      typeVars = map (\p -> PlainTV (mkName (T.unpack p)) BndrReq) params
      typeVarSet = Set.fromList params
      -- Generate field names with underscores
      mkFieldName f = mkName ("_" <> T.unpack name <> firstUpper (T.unpack (fdName f)))
  fieldDefs <- forM fields $ \f -> do
    fType <- compileTypeQ typeEnv typeVarSet (lVal (fdType f))
    pure (mkFieldName f, Bang NoSourceUnpackedness NoSourceStrictness, fType)
  let dataDec = DataD [] typeName typeVars Nothing
                [RecC typeName fieldDefs]
                (map (DerivClause Nothing . map ConT . pure . mkName . T.unpack) deriving')

  -- Generate lenses
  lenses <- mapM (generateLens typeEnv typeVarSet typeName) fields

  pure (dataDec : concat lenses)

-- | Generate a lens for a field
generateLens :: Set Text -> Set Text -> Name -> FieldDecl -> Q [Dec]
generateLens typeEnv typeVarSet typeName (FieldDecl fieldName fieldType) = do
  let lensName = mkName (T.unpack fieldName)
      getterName = mkName ("_" <> nameBase typeName <> firstUpper (T.unpack fieldName))
      
      -- lens definition: fieldName = lens _TypeFieldName (\s a -> s { _TypeFieldName = a })
      lensBody = AppE (AppE (VarE 'lens) (VarE getterName))
                   (LamE [VarP sName, VarP aName]
                     (RecUpdE (VarE sName) [(getterName, VarE aName)]))
      
      sName = mkName "s"
      aName = mkName "a"
      
  fieldTy <- compileTypeQ typeEnv typeVarSet (lVal fieldType)
  pure [SigD lensName (AppT (AppT (ConT ''Lens') (ConT typeName)) fieldTy),
        ValD (VarP lensName) (NormalB lensBody) []]

-- ** Newtype declarations

-- | Compile a newtype declaration
compileNewtypeDecl :: Set Text -> NewtypeDecl -> Q [Dec]
compileNewtypeDecl typeEnv (NewtypeDecl name params ty deriving') = do
  let typeName = mkName (T.unpack name)
      typeVars = map (\p -> PlainTV (mkName (T.unpack p)) BndrReq) params
      typeVarSet = Set.fromList params
      conName = typeName
  ty' <- compileTypeQ typeEnv typeVarSet (lVal ty)
  pure [NewtypeD [] typeName typeVars Nothing
     (NormalC conName [(Bang NoSourceUnpackedness NoSourceStrictness, ty')])
     (map (DerivClause Nothing . map ConT . pure . mkName . T.unpack) deriving')]

-- ** Enum declarations

-- | Compile an enum declaration (sum type)
compileEnumDecl :: Set Text -> EnumDecl -> Q [Dec]
compileEnumDecl typeEnv (EnumDecl name params variants deriving') = do
  let typeName = mkName (T.unpack name)
      typeVars = map (\p -> PlainTV (mkName (T.unpack p)) BndrReq) params
      typeVarSet = Set.fromList params
  cons <- forM variants $ \v -> do
    tys <- mapM (compileTypeQ typeEnv typeVarSet . lVal) (evFields v)
    pure (NormalC (mkName (T.unpack (evName v)))
            [ (Bang NoSourceUnpackedness NoSourceStrictness, ty)
            | ty <- tys
            ])

  pure [DataD [] typeName typeVars Nothing cons
         (map (DerivClause Nothing . map ConT . pure . mkName . T.unpack) deriving')]

-- ** Capability declarations

-- | Compile a capability declaration to a typeclass
compileCapDecl :: Set Text -> Set Text -> CapDecl -> Q [Dec]
compileCapDecl capEnv typeEnv (CapDecl capName super methods) = do
  let className = mkName ("Has" <> T.unpack capName)
      mVar = mkName "m"
      typeVarSet = Set.empty
  superCxt <- mapM (compileSuperConstraint capEnv typeEnv typeVarSet) super
  methodSigs <- mapM (\m -> SigD (mkName (capMethodName capName m)) <$> compileMethodType typeEnv typeVarSet mVar m) methods
      
  -- Typeclass declaration
  let classDec = ClassD superCxt className [PlainTV mVar BndrReq] []
                   methodSigs
  
  -- Helper function: requireCapName :: (Applicative m, HasCapName m) => m ()
  let requireName = mkName ("require" <> T.unpack capName)
      requireSig = SigD requireName
                     (ForallT [PlainTV mVar SpecifiedSpec]
                       [ AppT (ConT ''Applicative) (VarT mVar)
                       , AppT (ConT className) (VarT mVar)
                       ]
                       (AppT (VarT mVar) (TupleT 0)))
      requireDef = ValD (VarP requireName)
                     (NormalB (VarE 'pure `AppE` TupE []))
                     []
  
  pure [classDec, requireSig, requireDef]

-- | Compile a superclass constraint
compileSuperConstraint :: Set Text -> Set Text -> Set Text -> Constraint -> Q Pred
compileSuperConstraint capEnv typeEnv typeVarSet (Constraint cname args) =
  case cname of
    [capName] | null args && Set.member capName capEnv ->
      pure (AppT (ConT (mkName ("Has" <> T.unpack capName))) (VarT (mkName "m")))
    _ -> do
      baseName <- resolveTypeName typeEnv (last cname)
      argTypes <- mapM (compileTypeQ typeEnv typeVarSet . lVal) args
      pure (foldl' AppT (ConT baseName) (argTypes ++ [VarT (mkName "m")]))

-- | Generate capability method name: cap_capLower_method
capMethodName :: Text -> CapMethod -> String
capMethodName capName method =
  "cap_" <> firstLower (T.unpack capName) <> "_" <> T.unpack (cmName method)

-- | Compile method type signature
compileMethodType :: Set Text -> Set Text -> Name -> CapMethod -> Q TH.Type
compileMethodType typeEnv typeVarSet mVar method = do
  paramTypes <- mapM (compileTypeQ typeEnv typeVarSet . lVal . pType) (cmParams method)
  retType <- compileTypeQ typeEnv typeVarSet (lVal (cmReturn method))
  let fullType = foldr (\pt rest -> AppT (AppT ArrowT pt) rest)
                   (AppT (VarT mVar) retType)
                   paramTypes
  pure fullType

-- ** Function and procedure declarations

-- | Compile a function declaration (pure)
compileFnDecl :: OpEnv -> Set Text -> FnDecl -> Q [Dec]
compileFnDecl opEnv typeEnv (FnDecl name params retType _ body) = do
  let fnNameId = mkName (T.unpack name)
  ensureNoAwait name body
      
  -- Compile parameters
  paramPats <- mapM (const (newName "arg")) params
  
  -- Compile body (pure)
  bodyExp <- compileStmtsPure opEnv body
  let pureBody = AppE (VarE 'runIdentity) bodyExp
  let paramBinds =
        [ ValD (VarP (mkName (T.unpack (pName p)))) (NormalB (VarE argName)) []
        | (p, argName) <- zip params paramPats
        ]
      boundBody = if null paramBinds
        then bodyExp
        else LetE paramBinds bodyExp
  
  -- Build function
  let fnBodyExp = LamE (map VarP paramPats) (AppE (VarE 'runIdentity) boundBody)
      
  -- Type signature (if provided)
  paramTypes <- mapM (compileTypeQ typeEnv Set.empty . lVal . pType) params
  retType' <- maybe (pure (TupleT 0)) (compileTypeQ typeEnv Set.empty . lVal) retType
  let fnType = foldr (\pt rest -> AppT (AppT ArrowT pt) rest) retType' paramTypes
      
  pure [SigD fnNameId fnType, ValD (VarP fnNameId) (NormalB fnBodyExp) []]

-- | Compile a procedure declaration (effectful)
compileProcDecl :: OpEnv -> Set Text -> Set Text -> ProcDecl -> Q [Dec]
compileProcDecl opEnv capEnv typeEnv (ProcDecl name params retType requires' body) = do
  let procNameId = mkName (T.unpack name)
      mVar = mkName "m"
      
  -- Compile parameters
  paramPats <- mapM (const (newName "arg")) params
  
  -- Compile body with capability requirements
  let (capRequires, otherRequires) = partitionRequires capEnv requires'
      requireExprs = map compileRequire capRequires
  bodyExp <- compileStmtsWith opEnv body
  let paramBinds =
        [ ValD (VarP (mkName (T.unpack (pName p)))) (NormalB (VarE argName)) []
        | (p, argName) <- zip params paramPats
        ]
      boundBody = if null paramBinds
        then bodyExp
        else LetE paramBinds bodyExp
  
  let fullBody = if null requireExprs
                   then boundBody
                   else DoE Nothing (map NoBindS requireExprs ++ [NoBindS boundBody])
  
  -- Build procedure
  let procBodyExp = LamE (map VarP paramPats) fullBody
      
  -- Type signature
  paramTypes <- mapM (compileTypeQ typeEnv Set.empty . lVal . pType) params
  retType' <- maybe (pure (TupleT 0)) (compileTypeQ typeEnv Set.empty . lVal) retType
  capConstraints <- mapM (compileCapabilityConstraint capEnv mVar) capRequires
  otherConstraints <- mapM (compileConstraint typeEnv Set.empty mVar) otherRequires
  let procType = ForallT [PlainTV mVar SpecifiedSpec]
                   (AppT (ConT ''Monad) (VarT mVar) : capConstraints <> otherConstraints) $
                   foldr (\pt rest -> AppT (AppT ArrowT pt) rest)
                     (AppT (VarT mVar) retType')
                     paramTypes
      
  pure [SigD procNameId procType, ValD (VarP procNameId) (NormalB procBodyExp) []]

-- | Compile a require call
compileRequire :: Constraint -> Exp
compileRequire (Constraint cname _) =
  VarE (mkName ("require" <> T.unpack (last cname)))

-- | Compile a constraint
compileConstraint :: Set Text -> Set Text -> Name -> Constraint -> Q Pred
compileConstraint typeEnv typeVarSet mVar (Constraint cname args) = do
  baseName <- resolveTypeName typeEnv (last cname)
  argTypes <- mapM (compileTypeQ typeEnv typeVarSet . lVal) args
  pure (foldl' AppT (ConT baseName) (argTypes ++ [VarT mVar]))

compileCapabilityConstraint :: Set Text -> Name -> Constraint -> Q Pred
compileCapabilityConstraint capEnv mVar (Constraint cname _) = do
  let capName = last cname
  when (Set.notMember capName capEnv) $
    fail ("Unknown capability in requires: " <> T.unpack capName)
  pure (AppT (ConT (mkName ("Has" <> T.unpack capName))) (VarT mVar))

-- * Program compilation (imp quasiquoter)

-- | Compile a program (statement block) to an expression
compileProgram :: Program -> Q Exp
compileProgram (Program capHeader stmts) = do
  let capList = maybe [] (\(L _ (CapHeader caps)) -> caps) capHeader
  validateCapHeader capHeader
  let requireExprs = map (\c -> NoBindS (VarE (mkName ("require" <> T.unpack c)))) capList
  bodyExp <- compileStmtsWith defaultOpEnv stmts
  if null requireExprs
    then pure bodyExp
    else pure (DoE Nothing (requireExprs ++ [NoBindS bodyExp]))

-- * Statement compilation

-- | Compile a list of statements to an expression
compileStmtsWith :: OpEnv -> [L Stmt] -> Q Exp
compileStmtsWith _ [] = pure (VarE 'pure `AppE` TupE [])
compileStmtsWith opEnv stmts = DoE Nothing <$> mapM (compileStmtWith opEnv) stmts

-- | Compile a list of statements to a pure expression (Identity do-block)
compileStmtsPure :: OpEnv -> [L Stmt] -> Q Exp
compileStmtsPure _ [] = pure (VarE 'pure `AppE` TupE [])
compileStmtsPure opEnv stmts = DoE Nothing <$> mapM (compileStmtPure opEnv) stmts

-- | Compile a single statement in pure context
compileStmtPure :: OpEnv -> L Stmt -> Q TH.Stmt
compileStmtPure opEnv (L _ stmt) = case stmt of
  SVar name _ mval -> do
    let pat = VarP (mkName (T.unpack name))
    case mval of
      Nothing -> pure (LetS [ValD pat (NormalB (VarE 'undefined)) []])
      Just expr -> do
        e <- compileExprWith opEnv (lVal expr)
        pure (LetS [ValD pat (NormalB e) []])

  SRef name path -> do
    refExpr <- compileRefPath opEnv path
    pure (LetS [ValD (VarP (mkName (T.unpack name))) (NormalB refExpr) []])

  SAssign path op expr -> do
    e <- compileExprWith opEnv (lVal expr)
    case path of
      RefPath (RName name) [] -> do
        assigned <- assignValue opEnv op (VarE (mkName (T.unpack name))) e
        pure (LetS [ValD (VarP (mkName (T.unpack name))) (NormalB assigned) []])
      _ -> do
        refExpr <- compileRefPath opEnv path
        assigned <- assignRef opEnv op refExpr e
        pure (NoBindS (VarE 'pure `AppE` assigned))

  SExpr expr -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (NoBindS (VarE 'pure `AppE` e))

  SReturn Nothing -> pure (NoBindS (VarE 'pure `AppE` TupE []))
  SReturn (Just expr) -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (NoBindS (VarE 'pure `AppE` e))

  SAwait{} -> fail "await is not allowed in pure fn bodies"

  SIf cond thenStmts elseStmts -> do
    condExp <- compileExprWith opEnv (lVal cond)
    thenExp <- compileStmtsPure opEnv thenStmts
    elseExp <- compileStmtsPure opEnv (fromMaybe [] elseStmts)
    pure (NoBindS (CondE condExp thenExp elseExp))

  SWhile cond body -> do
    condExp <- compileExprWith opEnv (lVal cond)
    bodyExp <- compileStmtsPure opEnv body
    let loopName = mkName "loop"
        loopBody = DoE Nothing
            [ NoBindS (CondE condExp
              (DoE Nothing [NoBindS bodyExp, NoBindS (VarE loopName)])
              (VarE 'pure `AppE` TupE []))
          ]
        loopDec = LetS [ValD (VarP loopName) (NormalB loopBody) []]
    pure (NoBindS (DoE Nothing [loopDec, NoBindS (VarE loopName)]))

  SFor init' cond incr body -> do
    initStmt <- mapM (compileStmtPure opEnv) init'
    condExp <- maybe (pure (ConE 'True)) (compileExprWith opEnv . lVal) cond
    incrExp <- mapM (compileExprWith opEnv . lVal) incr
    bodyExp <- compileStmtsPure opEnv body
    let loopName = mkName "loop"
        incrStmt = maybe [] (\e -> [NoBindS (VarE 'pure `AppE` e)]) incrExp
        loopBody = DoE Nothing
            [ NoBindS (CondE condExp
              (DoE Nothing ([NoBindS bodyExp] ++ incrStmt ++ [NoBindS (VarE loopName)]))
              (VarE 'pure `AppE` TupE []))
          ]
        loopDec = LetS [ValD (VarP loopName) (NormalB loopBody) []]
        runLoop = NoBindS (VarE loopName)
        initStmts = maybe [] pure initStmt
    pure (NoBindS (DoE Nothing (initStmts ++ [loopDec, runLoop])))

  SSwitch expr cases def -> do
    scrut <- compileExprWith opEnv (lVal expr)
    compiled <- mapM (compileSwitchCase opEnv scrut) cases
    defExp <- compileStmtsPure opEnv (fromMaybe [] def)
    let mkCase compiled acc = CondE (caseCond compiled) (caseBody compiled) acc
        chain = foldr mkCase defExp compiled
    pure (NoBindS chain)

  SThrow expr -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (NoBindS (VarE 'pure `AppE` e))

  STry body catches fin -> do
    bodyExp <- compileStmtsPure opEnv body
    catchExps <- mapM (\(_, _, stmts) -> compileStmtsPure opEnv stmts) catches
    finExp <- compileStmtsPure opEnv (fromMaybe [] fin)
    let combined = foldr (\c acc -> CondE (ConE 'True) c acc) bodyExp catchExps
    pure (NoBindS (CondE (ConE 'True) combined finExp))

  SBlock stmts -> do
    bodyExp <- compileStmtsPure opEnv stmts
    pure (NoBindS bodyExp)

  SBreak -> pure (NoBindS (VarE 'pure `AppE` TupE []))
  SContinue -> pure (NoBindS (VarE 'pure `AppE` TupE []))

-- | Compile a single statement
compileStmtWith :: OpEnv -> L Stmt -> Q TH.Stmt
compileStmtWith opEnv (L _ stmt) = case stmt of
  SVar name _ mval -> do
    let pat = VarP (mkName (T.unpack name))
    case mval of
      Nothing -> pure (LetS [ValD pat (NormalB (VarE 'undefined)) []])
      Just expr -> do
        e <- compileExprWith opEnv (lVal expr)
        pure (LetS [ValD pat (NormalB e) []])

  SRef name path -> do
    refExpr <- compileRefPath opEnv path
    pure (LetS [ValD (VarP (mkName (T.unpack name))) (NormalB refExpr) []])

  SAssign path op expr -> do
    e <- compileExprWith opEnv (lVal expr)
    case path of
      RefPath (RName name) [] ->
        do
          assigned <- assignValue opEnv op (VarE (mkName (T.unpack name))) e
          pure (LetS [ValD (VarP (mkName (T.unpack name))) (NormalB assigned) []])
      _ -> do
        refExpr <- compileRefPath opEnv path
        assigned <- assignRef opEnv op refExpr e
        pure (NoBindS assigned)

  SExpr expr -> NoBindS <$> compileExprWith opEnv (lVal expr)

  SReturn Nothing -> pure (NoBindS (VarE 'pure `AppE` TupE []))
  SReturn (Just expr) -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (NoBindS (VarE 'pure `AppE` e))

  SAwait Nothing expr -> NoBindS <$> compileExprWith opEnv (lVal expr)
  SAwait (Just name) expr -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (BindS (VarP (mkName (T.unpack name))) e)

  SIf cond thenStmts elseStmts -> do
    condExp <- compileExprWith opEnv (lVal cond)
    thenExp <- compileStmtsWith opEnv thenStmts
    elseExp <- compileStmtsWith opEnv (fromMaybe [] elseStmts)
    pure (NoBindS (CondE condExp thenExp elseExp))

  SWhile cond body -> do
    condExp <- compileExprWith opEnv (lVal cond)
    bodyExp <- compileStmtsWith opEnv body
    let loopName = mkName "loop"
        loopBody = DoE Nothing
            [ NoBindS (CondE condExp
              (DoE Nothing [NoBindS bodyExp, NoBindS (VarE loopName)])
              (VarE 'pure `AppE` TupE []))
          ]
        loopDec = LetS [ValD (VarP loopName) (NormalB loopBody) []]
    pure (NoBindS (DoE Nothing [loopDec, NoBindS (VarE loopName)]))

  SFor init' cond incr body -> do
    initStmt <- mapM (compileStmtWith opEnv) init'
    condExp <- maybe (pure (ConE 'True)) (compileExprWith opEnv . lVal) cond
    incrExp <- mapM (compileExprWith opEnv . lVal) incr
    bodyExp <- compileStmtsWith opEnv body
    let loopName = mkName "loop"
        incrStmt = maybe [] (\e -> [NoBindS (VarE 'pure `AppE` e)]) incrExp
        loopBody = DoE Nothing
            [ NoBindS (CondE condExp
              (DoE Nothing ([NoBindS bodyExp] ++ incrStmt ++ [NoBindS (VarE loopName)]))
              (VarE 'pure `AppE` TupE []))
          ]
        loopDec = LetS [ValD (VarP loopName) (NormalB loopBody) []]
        runLoop = NoBindS (VarE loopName)
        initStmts = maybe [] pure initStmt
    pure (NoBindS (DoE Nothing (initStmts ++ [loopDec, runLoop])))

  SSwitch expr cases def -> do
    scrut <- compileExprWith opEnv (lVal expr)
    defExp <- compileStmtsWith opEnv (fromMaybe [] def)
    caseExps <- mapM (compileSwitchCase opEnv scrut) cases
    let chain = foldr (\caseExp acc -> CondE (caseCond caseExp) (caseBody caseExp) acc) defExp caseExps
    pure (NoBindS chain)

  SBreak -> pure (NoBindS (VarE 'pure `AppE` TupE []))
  SContinue -> pure (NoBindS (VarE 'pure `AppE` TupE []))

  SThrow expr -> do
    e <- compileExprWith opEnv (lVal expr)
    pure (NoBindS (AppE (VarE 'error) (AppE (VarE 'show) e)))

  STry body _ _ -> do
    bodyExp <- compileStmtsWith opEnv body
    pure (NoBindS bodyExp)

  SBlock stmts -> NoBindS <$> compileStmtsWith opEnv stmts

-- * Expression compilation

-- | Compile an expression to Template Haskell
compileExpr :: Expr -> Q Exp
compileExpr = compileExprWith defaultOpEnv

compileExprWith :: OpEnv -> Expr -> Q Exp
compileExprWith opEnv expr = case expr of
  EVar name
    | not (T.null name) && isUpper (T.head name) -> ConE <$> resolveValueName [name]
    | otherwise -> VarE <$> resolveValueName [name]

  EThis -> pure (VarE (mkName "this"))

  ELit lit -> compileLit lit

  ECall (L _ (EMember obj field)) args -> do
    o <- compileExprWith opEnv (lVal obj)
    as <- mapM (compileExprWith opEnv . lVal) args
    fName <- resolveValueName [field]
    let f = foldl' AppE (VarE fName) (o : as)
    pure f

  ECall func args -> do
    f <- compileExprWith opEnv (lVal func)
    as <- mapM (compileExprWith opEnv . lVal) args
    pure (foldl' AppE f as)

  EMember obj field -> do
    o <- compileExprWith opEnv (lVal obj)
    fieldName <- resolveValueName [field]
    pure (AppE (AppE (VarE 'view) (VarE fieldName)) o)

  ERecordUpdate obj updates -> do
    base <- compileExprWith opEnv (lVal obj)
    compiled <- mapM (compileRecordUpdateField opEnv) updates
    pure (foldl' applyUpdate base compiled)
    where
      applyUpdate acc (fieldName, valueExp) =
        AppE (AppE (AppE (VarE 'set) (VarE fieldName)) valueExp) acc
  
  EBinOp op left right -> do
    l <- compileExprWith opEnv (lVal left)
    r <- compileExprWith opEnv (lVal right)
    opName <- resolveBinOpName opEnv op
    pure (InfixE (Just l) (VarE opName) (Just r))
  
  EUnOp op arg -> do
    a <- compileExprWith opEnv (lVal arg)
    pure (AppE (VarE (unopToName op)) a)
  
  ECapCall cap method args -> do
    as <- mapM (compileExprWith opEnv . lVal) args
    let methodName = mkName ("cap_" <> firstLower (T.unpack cap) <> "_" <> T.unpack method)
    pure (foldl' AppE (VarE methodName) as)
  
  EIndex obj idx -> do
    o <- compileExprWith opEnv (lVal obj)
    i <- compileExprWith opEnv (lVal idx)
    pure (AppE (AppE (VarE 'impIndex) o) i)

  ENew ty args -> do
    conName <- resolveConstructorName ty
    as <- mapM (compileExprWith opEnv . lVal) args
    pure (foldl' AppE (ConE conName) as)

  EHs code -> parseHaskellExp (T.unpack code)

  EHsM code -> parseHaskellExp (T.unpack code)

compileRecordUpdateField :: OpEnv -> (Text, L Expr) -> Q (Name, Exp)
compileRecordUpdateField opEnv (field, expr) = do
  fieldName <- resolveValueName [field]
  valueExp <- compileExprWith opEnv (lVal expr)
  pure (fieldName, valueExp)

-- | Compile a literal
compileLit :: Lit -> Q Exp
compileLit lit = case lit of
  LInt n -> pure (LitE (IntegerL n))
  LDouble d -> pure (LitE (RationalL (toRational d)))
  LText t -> pure (LitE (StringL (T.unpack t)))
  LBool True -> pure (ConE 'True)
  LBool False -> pure (ConE 'False)
  LNull -> pure (ConE 'Nothing)
  LTagged tag inner -> do
    innerExp <- compileLit inner
    pure (AppE (AppE (VarE (mkName "taggedLiteral")) (LitE (StringL (T.unpack tag)))) innerExp)

-- * Type compilation

-- | Compile a DSL type to a Haskell Type
compileTypeQ :: Set Text -> Set Text -> Type -> Q TH.Type
compileTypeQ typeEnv typeVarSet ty = case ty of
  TPrim pt -> pure (compilePrimType pt)
  TUser [name]
    | name `Set.member` typeVarSet -> pure (VarT (mkName (T.unpack name)))
    | name `Set.member` typeEnv -> pure (ConT (mkName (T.unpack name)))
    | otherwise -> ConT <$> resolveTypeName typeEnv name
  TUser names -> ConT <$> resolveQualifiedTypeName typeEnv names
  TOption t -> AppT (ConT ''Maybe) <$> compileTypeQ typeEnv typeVarSet (lVal t)
  TList t -> AppT ListT <$> compileTypeQ typeEnv typeVarSet (lVal t)
  TMap k v -> do
    kType <- compileTypeQ typeEnv typeVarSet (lVal k)
    vType <- compileTypeQ typeEnv typeVarSet (lVal v)
    pure (AppT (AppT (ConT ''Map) kType) vType)
  TGeneric name args -> do
    base <- if name `Set.member` typeVarSet
              then pure (VarT (mkName (T.unpack name)))
              else ConT <$> resolveTypeName typeEnv name
    argTypes <- mapM (compileTypeQ typeEnv typeVarSet . lVal) args
    pure (foldl' AppT base argTypes)

-- | Compile a primitive type
compilePrimType :: PrimType -> TH.Type
compilePrimType pt = case pt of
  PBool   -> ConT ''Bool
  PChar   -> ConT ''Char
  PString -> ConT ''String
  PText   -> ConT ''Text
  PByte   -> ConT ''Word8
  PShort  -> ConT ''Int16
  PInt    -> ConT ''Int32
  PLong   -> ConT ''Int64
  PFloat  -> ConT ''Float
  PDouble -> ConT ''Double
  PBytes  -> ConT ''ByteString
  PUnit   -> TupleT 0
  PUuid   -> ConT ''UUID

-- * Operator name mapping

-- | Map binary operator to Haskell name
opToName :: BinOp -> Name
opToName op = case op of
  OpAdd -> '(+)
  OpSub -> '(-)
  OpMul -> '(*)
  OpDiv -> '(/)
  OpMod -> 'mod
  OpEq  -> '(==)
  OpNeq -> '(/=)
  OpLt  -> '(<)
  OpLte -> '(<=)
  OpGt  -> '(>)
  OpGte -> '(>=)
  OpAnd -> '(&&)
  OpOr  -> '(||)
  OpBitAnd -> '(.&.)
  OpBitOr  -> '(.|.)
  OpBitXor -> 'xor
  OpShiftL -> 'shiftL
  OpShiftR -> 'shiftR

-- | Map unary operator to Haskell name
unopToName :: UnOp -> Name
unopToName op = case op of
  OpNot    -> 'not
  OpNeg    -> 'negate
  OpBitNot -> 'complement

-- * Operator environment

data OpTarget
  = OpTargetName Name
  | OpTargetQualified [Text]
  deriving (Show, Eq)

type OpEnv = Map BinOp OpTarget

defaultOpEnv :: OpEnv
defaultOpEnv = Map.fromList
  [ (OpAdd, OpTargetName 'impAdd)
  , (OpSub, OpTargetName (opToName OpSub))
  , (OpMul, OpTargetName (opToName OpMul))
  , (OpDiv, OpTargetName (opToName OpDiv))
  , (OpMod, OpTargetName (opToName OpMod))
  , (OpEq, OpTargetName (opToName OpEq))
  , (OpNeq, OpTargetName (opToName OpNeq))
  , (OpLt, OpTargetName (opToName OpLt))
  , (OpLte, OpTargetName (opToName OpLte))
  , (OpGt, OpTargetName (opToName OpGt))
  , (OpGte, OpTargetName (opToName OpGte))
  , (OpAnd, OpTargetName (opToName OpAnd))
  , (OpOr, OpTargetName (opToName OpOr))
  , (OpBitAnd, OpTargetName (opToName OpBitAnd))
  , (OpBitOr, OpTargetName (opToName OpBitOr))
  , (OpBitXor, OpTargetName (opToName OpBitXor))
  , (OpShiftL, OpTargetName (opToName OpShiftL))
  , (OpShiftR, OpTargetName (opToName OpShiftR))
  ]

applyOpBlock :: OpEnv -> OpBlock -> OpEnv
applyOpBlock opEnv (OpBlock mappings) =
  foldl' applyMapping opEnv mappings
  where
    applyMapping env (op, names) =
      case names of
        [] -> env
        _  -> Map.insert op (OpTargetQualified names) env

-- * Utility functions

-- | Parse raw Haskell code as declarations
parseHaskellDecs :: Text -> Q [Dec]
parseHaskellDecs code =
  case Meta.parseDecs (T.unpack code) of
    Left err -> fail ("Failed to parse raw Haskell declarations: " <> err)
    Right decs -> pure decs

-- | Parse raw Haskell code as expression
parseHaskellExp :: String -> Q Exp
parseHaskellExp code =
  case Meta.parseExp code of
    Left err -> fail ("Failed to parse raw Haskell expression: " <> err)
    Right expr -> pure expr

-- | First character to uppercase
firstUpper :: String -> String
firstUpper [] = []
firstUpper (c:cs) = toUpper c : cs

-- | First character to lowercase
firstLower :: String -> String
firstLower [] = []
firstLower (c:cs) = toLower c : cs

-- * Name resolution helpers

resolveQualifiedTypeName :: Set Text -> [Text] -> Q Name
resolveQualifiedTypeName typeEnv parts =
  resolveTypeName typeEnv (T.intercalate "." parts)

resolveTypeName :: Set Text -> Text -> Q Name
resolveTypeName typeEnv name
  | name `Set.member` typeEnv = pure (mkName (T.unpack name))
  | otherwise = do
      let raw = T.unpack name
      found <- lookupTypeName raw
      case found of
        Just n -> pure n
        Nothing -> pure (mkName raw)

resolveConstructorName :: L Type -> Q Name
resolveConstructorName (L _ ty) = case ty of
  TUser [name] -> resolveValueName [name]
  TUser names -> resolveValueName names
  TGeneric name _ -> resolveValueName [name]
  _ -> fail "Unsupported constructor target in new expression"

-- | Resolve a value name with a best-effort lookup for qualified names.
resolveValueName :: [Text] -> Q Name
resolveValueName parts = do
  let raw = T.unpack (T.intercalate "." parts)
  found <- lookupValueName raw
  case found of
    Just n -> pure n
    Nothing ->
      if length parts == 1
        then pure (mkName raw)
        else fail ("Unknown value name: " <> raw)

resolveBinOpName :: OpEnv -> BinOp -> Q Name
resolveBinOpName opEnv op =
  case Map.findWithDefault (OpTargetName (opToName op)) op opEnv of
    OpTargetName n -> pure n
    OpTargetQualified parts -> resolveValueName parts

-- * Statement helpers

assignValue :: OpEnv -> AssignOp -> Exp -> Exp -> Q Exp
assignValue opEnv op lhs rhs =
  case op of
    AEq -> pure rhs
    _ -> do
      opName <- resolveAssignOpName opEnv op
      pure (InfixE (Just lhs) (VarE opName) (Just rhs))

assignRef :: OpEnv -> AssignOp -> Exp -> Exp -> Q Exp
assignRef opEnv op refExpr rhs =
  case op of
    AEq -> pure (AppE (AppE (VarE 'setRef) refExpr) rhs)
    _ -> do
      opName <- resolveAssignOpName opEnv op
      let xName = mkName "x"
          lamBody = InfixE (Just (VarE xName)) (VarE opName) (Just rhs)
      pure (AppE (AppE (VarE 'modRef) refExpr) (LamE [VarP xName] lamBody))

resolveAssignOpName :: OpEnv -> AssignOp -> Q Name
resolveAssignOpName opEnv op =
  let binOp = case op of
        APlusEq -> OpAdd
        AMinusEq -> OpSub
        AMulEq -> OpMul
        ADivEq -> OpDiv
        AModEq -> OpMod
        AEq -> OpAdd
  in resolveBinOpName opEnv binOp

compileRefPath :: OpEnv -> RefPath -> Q Exp
compileRefPath opEnv (RefPath root parts) = case root of
  RThis -> do
    when (null parts) (fail "ref path cannot target `this` directly")
    lensChain <- compileLensChain opEnv parts
    pure (AppE (ConE 'Ref) lensChain)
  RName name -> do
    let base = VarE (mkName (T.unpack name))
    if null parts
      then pure base
      else do
        lensParts <- mapM (compileRefPart opEnv) parts
        let refChain = foldl' (\acc l -> AppE (AppE (VarE '(.^)) acc) l) base lensParts
        pure refChain

compileLensChain :: OpEnv -> [RefPart] -> Q Exp
compileLensChain opEnv parts = do
  lensParts <- mapM (compileRefPart opEnv) parts
  case lensParts of
    [] -> fail "empty lens chain"
    [l] -> pure l
    (l:ls) -> pure (foldl' (\acc l' -> InfixE (Just acc) (VarE '(.) ) (Just l')) l ls)

compileRefPart :: OpEnv -> RefPart -> Q Exp
compileRefPart opEnv part = case part of
  PField name -> pure (VarE (mkName (T.unpack name)))
  PIndex expr -> do
    idx <- compileExprWith opEnv (lVal expr)
    pure (AppE (VarE 'ix) (AppE (VarE 'fromIntegral) idx))

-- * Switch helpers

data CompiledCase = CompiledCase
  { caseCond :: Exp
  , caseBody :: Exp
  }

compileSwitchCase :: OpEnv -> Exp -> SwitchCase -> Q CompiledCase
compileSwitchCase opEnv scrut (SwitchCase val stmts) = do
  v <- compileExprWith opEnv (lVal val)
  body <- compileStmtsWith opEnv stmts
  eqName <- resolveBinOpName opEnv OpEq
  pure (CompiledCase
    { caseCond = InfixE (Just scrut) (VarE eqName) (Just v)
    , caseBody = body
    })

-- * Validation helpers

validateCapHeader :: Maybe (L CapHeader) -> Q ()
validateCapHeader capHeader =
  case capHeader of
    Nothing -> pure ()
    Just (L (Span start _) (CapHeader caps)) -> do
      let dupes = caps \\ Set.toList (Set.fromList caps)
      when (not (null dupes)) $ do
        let line = unPos (sourceLine start)
            col = unPos (sourceColumn start)
        fail ("Duplicate capability entries in header at line " <> show line <> " col " <> show col)

ensureNoAwait :: Text -> [L Stmt] -> Q ()
ensureNoAwait fnLabel stmts =
  when (any stmtHasAwait stmts) $
    fail ("await is not allowed in fn: " <> T.unpack fnLabel)

stmtHasAwait :: L Stmt -> Bool
stmtHasAwait (L _ stmt) = case stmt of
  SAwait{} -> True
  SIf _ t f -> any stmtHasAwait t || maybe False (any stmtHasAwait) f
  SFor init' _ _ body -> maybe False stmtHasAwait init' || any stmtHasAwait body
  SWhile _ body -> any stmtHasAwait body
  SSwitch _ cases def -> any caseHasAwait cases || maybe False (any stmtHasAwait) def
  STry body catches fin ->
    any stmtHasAwait body
      || any (any stmtHasAwait . (\(_, _, s) -> s)) catches
      || maybe False (any stmtHasAwait) fin
  SBlock xs -> any stmtHasAwait xs
  _ -> False

caseHasAwait :: SwitchCase -> Bool
caseHasAwait (SwitchCase _ stmts) = any stmtHasAwait stmts

partitionRequires :: Set Text -> [Constraint] -> ([Constraint], [Constraint])
partitionRequires capEnv =
  foldr (\c@(Constraint cname args) (caps, others) ->
           case cname of
             [cap] | null args && cap `Set.member` capEnv -> (c : caps, others)
             _ -> (caps, c : others)) ([], [])
