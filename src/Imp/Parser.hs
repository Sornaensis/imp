{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec parser for the imperative DSL
module Imp.Parser
  ( -- * Parsers
    parseModule
  , parseProgram
  , parseExpr
    -- * Parser type
  , Parser
  , ImpParseError
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Imp.AST

-- * Parser type

type Parser = Parsec Void Text
type ImpParseError = ParseErrorBundle Text Void

-- * Lexer

-- | Space consumer (whitespace and comments)
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse in braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse in brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse in angle brackets
angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

-- | Parse a semicolon
semi :: Parser ()
semi = void (symbol ";")

-- | Parse a comma
comma :: Parser ()
comma = void (symbol ",")

-- | Reserved words
reserved :: [Text]
reserved =
  [ "capability", "requires", "operators"
  , "type", "newtype", "enum", "fn", "proc"
  , "capabilities", "await", "var", "ref", "this"
  , "if", "else", "for", "while", "switch", "case", "default"
  , "break", "continue", "return", "throw", "try", "catch", "finally"
  , "new", "null", "true", "false"
  , "deriving"
  -- primitive types
  , "bool", "char", "string", "text", "byte", "short", "int", "long"
  , "float", "double", "bytes", "unit", "void", "uuid"
  , "option", "list", "map"
  , "Unit", "Void", "Option", "List", "Map"
  ]

-- | Parse an identifier
ident :: Parser Text
ident = (lexeme . try) $ do
  x <- T.pack <$> ((:) <$> satisfy isIdentStart <*> many (satisfy isIdentChar))
  if x `elem` reserved
    then fail ("reserved word: " <> T.unpack x)
    else pure x
  where
    isIdentStart c = isLetter c || c == '_'
    isIdentChar c = isAlphaNum c || c == '_'

-- | Parse an identifier without reserved-word checks (for tagged literals)
rawIdent :: Parser Text
rawIdent = lexeme $ do
  T.pack <$> ((:) <$> satisfy isIdentStart <*> many (satisfy isIdentChar))
  where
    isIdentStart c = isLetter c || c == '_'
    isIdentChar c = isAlphaNum c || c == '_'

-- | Parse a qualified identifier (dot-separated)
qualIdent :: Parser [Text]
qualIdent = ident `sepBy1` symbol "."

-- * Balanced brace blocks for raw Haskell

-- | Parse balanced braces and return the content
balancedBraces :: Parser Text
balancedBraces = do
  void (char '{')
  T.pack <$> go 1 []
  where
    go :: Int -> String -> Parser String
    go 0 acc = pure (reverse acc)
    go n acc = do
      c <- anySingle
      case c of
        '{' -> go (n + 1) (c : acc)
        '}' -> if n == 1
                 then pure (reverse acc)  -- don't include final '}'
                 else go (n - 1) (c : acc)
        _   -> go n (c : acc)

-- | Parse hs{...} block
hsExprTok :: Parser Text
hsExprTok = lexeme $ do
  void (string "hs")
  sc
  balancedBraces

-- | Parse hsM{...} block
hsMExprTok :: Parser Text
hsMExprTok = lexeme $ do
  void (string "hsM")
  sc
  balancedBraces

-- | Parse #{...} block for declarations
hsDecBlock :: Parser Text
hsDecBlock = lexeme $ do
  void (string "#{")
  T.pack <$> go 1 []
  where
    go :: Int -> String -> Parser String
    go 0 acc = pure (reverse acc)
    go n acc = do
      c <- anySingle
      case c of
        '{' -> go (n + 1) (c : acc)
        '}' -> if n == 1
                 then pure (reverse acc)
                 else go (n - 1) (c : acc)
        _   -> go n (c : acc)

-- * Located node wrapper

-- | Parse with span tracking
withSpan :: Parser a -> Parser (L a)
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pure (L (Span start end) x)

-- * Type parsers

-- | Parse a type
parseType :: Parser (L Type)
parseType = withSpan $ choice
  [ try pOptionType
  , try pListType
  , try pMapType
  , try pPrimType
  , TUser <$> qualIdent
  ]

-- | Parse a primitive type
pPrimType :: Parser Type
pPrimType = TPrim <$> choice
  [ PBool   <$ symbol "bool"
  , PChar   <$ symbol "char"
  , PString <$ symbol "string"
  , PText   <$ symbol "text"
  , PByte   <$ symbol "byte"
  , PShort  <$ symbol "short"
  , PInt    <$ symbol "int"
  , PLong   <$ symbol "long"
  , PFloat  <$ symbol "float"
  , PDouble <$ symbol "double"
  , PBytes  <$ symbol "bytes"
  , PUnit   <$ (symbol "unit" <|> symbol "void" <|> symbol "Unit" <|> symbol "Void")
  , PUuid   <$ symbol "uuid"
  ]

-- | Parse option<T>
pOptionType :: Parser Type
pOptionType = do
  void (symbol "option" <|> symbol "Option")
  TOption <$> angles parseType

-- | Parse list<T>
pListType :: Parser Type
pListType = do
  void (symbol "list" <|> symbol "List")
  TList <$> angles parseType

-- | Parse map<K,V>
pMapType :: Parser Type
pMapType = do
  void (symbol "map" <|> symbol "Map")
  angles $ do
    k <- parseType
    comma
    v <- parseType
    pure (TMap k v)

-- * Expression parsers

-- | Parse an expression with precedence using manual precedence climbing
pExpr :: Parser (L Expr)
pExpr = withSpan pOrExpr

-- Precedence climbing: lowest to highest
pOrExpr :: Parser Expr
pOrExpr = do
  left <- pAndExpr
  rest <- many ((,) <$> symbol "||" <*> pAndExpr)
  pure $ foldl' (\l (_, r) -> EBinOp OpOr (dummyL l) (dummyL r)) left rest

pAndExpr :: Parser Expr
pAndExpr = do
  left <- pBitOrExpr
  rest <- many ((,) <$> symbol "&&" <*> pBitOrExpr)
  pure $ foldl' (\l (_, r) -> EBinOp OpAnd (dummyL l) (dummyL r)) left rest

pBitOrExpr :: Parser Expr
pBitOrExpr = do
  left <- pBitXorExpr
  rest <- many ((,) <$> symbol "|" <*> pBitXorExpr)
  pure $ foldl' (\l (_, r) -> EBinOp OpBitOr (dummyL l) (dummyL r)) left rest

pBitXorExpr :: Parser Expr
pBitXorExpr = do
  left <- pBitAndExpr
  rest <- many ((,) <$> symbol "^" <*> pBitAndExpr)
  pure $ foldl' (\l (_, r) -> EBinOp OpBitXor (dummyL l) (dummyL r)) left rest

pBitAndExpr :: Parser Expr
pBitAndExpr = do
  left <- pEqExpr
  rest <- many ((,) <$> symbol "&" <*> pEqExpr)
  pure $ foldl' (\l (_, r) -> EBinOp OpBitAnd (dummyL l) (dummyL r)) left rest

pEqExpr :: Parser Expr
pEqExpr = do
  left <- pRelExpr
  rest <- optional $ choice
    [ (,) OpEq <$> (symbol "==" *> pRelExpr)
    , (,) OpNeq <$> (symbol "!=" *> pRelExpr)
    ]
  case rest of
    Nothing -> pure left
    Just (op, right) -> pure (EBinOp op (dummyL left) (dummyL right))

pRelExpr :: Parser Expr
pRelExpr = do
  left <- pShiftExpr
  rest <- optional $ choice
    [ (,) OpLte <$> try (symbol "<=" *> pShiftExpr)
    , (,) OpLt <$> (symbol "<" *> pShiftExpr)
    , (,) OpGte <$> try (symbol ">=" *> pShiftExpr)
    , (,) OpGt <$> (symbol ">" *> pShiftExpr)
    ]
  case rest of
    Nothing -> pure left
    Just (op, right) -> pure (EBinOp op (dummyL left) (dummyL right))

pShiftExpr :: Parser Expr
pShiftExpr = do
  left <- pAddExpr
  rest <- many $ choice
    [ (,) OpShiftL <$> (symbol "<<" *> pAddExpr)
    , (,) OpShiftR <$> (symbol ">>" *> pAddExpr)
    ]
  pure $ foldl' (\l (op, r) -> EBinOp op (dummyL l) (dummyL r)) left rest

pAddExpr :: Parser Expr
pAddExpr = do
  left <- pMulExpr
  rest <- many $ choice
    [ (,) OpAdd <$> (symbol "+" *> pMulExpr)
    , (,) OpSub <$> (symbol "-" *> pMulExpr)
    ]
  pure $ foldl' (\l (op, r) -> EBinOp op (dummyL l) (dummyL r)) left rest

pMulExpr :: Parser Expr
pMulExpr = do
  left <- pUnaryExpr
  rest <- many $ choice
    [ (,) OpMul <$> (symbol "*" *> pUnaryExpr)
    , (,) OpDiv <$> (symbol "/" *> pUnaryExpr)
    , (,) OpMod <$> (symbol "%" *> pUnaryExpr)
    ]
  pure $ foldl' (\l (op, r) -> EBinOp op (dummyL l) (dummyL r)) left rest

pUnaryExpr :: Parser Expr
pUnaryExpr = choice
  [ do
      void (symbol "!")
      e <- pUnaryExpr
      pure (EUnOp OpNot (dummyL e))
  , do
      void (symbol "-")
      e <- pUnaryExpr
      pure (EUnOp OpNeg (dummyL e))
  , do
      start <- getSourcePos
      e <- pPrimary
      let locExpr = L (Span start start) e
      lVal <$> pPostfix locExpr
  ]

-- | Create a dummy located value (for internal use during parsing)
dummyL :: a -> L a
dummyL a = L (Span (initialPos "<dummy>") (initialPos "<dummy>")) a

-- | Parse a primary expression
pPrimary :: Parser Expr
pPrimary = choice
  [ try pCapCall
  , EThis <$ symbol "this"
  , try (EHsM <$> hsMExprTok)
  , try (EHs <$> hsExprTok)
  , ELit <$> pLit
  , try pNewExpr
  , EVar <$> ident
  , parens (lVal <$> withSpan pOrExpr)
  ]

-- | Parse a capability call: capabilities.cap.method(args)
pCapCall :: Parser Expr
pCapCall = do
  void (symbol "capabilities")
  void (symbol ".")
  cap <- ident
  void (symbol ".")
  meth <- ident
  args <- parens (withSpan pOrExpr `sepBy` comma)
  pure (ECapCall cap meth args)

-- | Parse a new expression: new Type(args)
pNewExpr :: Parser Expr
pNewExpr = do
  void (symbol "new")
  ty <- parseType
  args <- parens (withSpan pOrExpr `sepBy` comma)
  pure (ENew ty args)

-- | Parse postfix operations (method call, member access, indexing)
pPostfix :: L Expr -> Parser (L Expr)
pPostfix e = go e
  where
    go expr = choice
      [ do
          void (symbol ".")
          field <- ident
          end <- getSourcePos
          let newExpr = L (Span (spanStart (lSpan expr)) end) (EMember expr field)
          go newExpr
      , do
          idx <- brackets (withSpan pOrExpr)
          end <- getSourcePos
          let newExpr = L (Span (spanStart (lSpan expr)) end) (EIndex expr idx)
          go newExpr
      , do
          args <- parens (withSpan pOrExpr `sepBy` comma)
          end <- getSourcePos
          let newExpr = L (Span (spanStart (lSpan expr)) end) (ECall expr args)
          go newExpr
      , pure expr
      ]

-- | Parse a literal
pLit :: Parser Lit
pLit = choice
  [ try pTaggedLit
  , pLitCore
  ]

-- | Parse a tagged literal: tag#<literal>
pTaggedLit :: Parser Lit
pTaggedLit = do
  tag <- rawIdent
  void (symbol "#")
  lit <- pLitCore
  pure (LTagged tag lit)

-- | Parse a core literal (untagged)
pLitCore :: Parser Lit
pLitCore = choice
  [ LBool True <$ symbol "true"
  , LBool False <$ symbol "false"
  , LNull <$ symbol "null"
  , try pDoubleLit
  , pIntLit
  , pTextLit
  ]

-- | Parse an integer literal
pIntLit :: Parser Lit
pIntLit = lexeme $ LInt <$> L.decimal

-- | Parse a double literal
pDoubleLit :: Parser Lit
pDoubleLit = lexeme $ LDouble <$> L.float

-- | Parse a text literal
pTextLit :: Parser Lit
pTextLit = LText . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- * Statement parsers

-- | Parse a statement
pStmt :: Parser (L Stmt)
pStmt = withSpan $ choice
  [ try pVarStmt
  , try pRefStmt
  , try pAssignStmt
  , try pAwaitStmt
  , pIfStmt
  , pForStmt
  , pWhileStmt
  , pSwitchStmt
  , pReturnStmt
  , pBreakStmt
  , pContinueStmt
  , pThrowStmt
  , pTryStmt
  , SBlock <$> braces (many pStmt)
  , SExpr <$> pExpr <* semi
  ]

-- | Parse var declaration
pVarStmt :: Parser Stmt
pVarStmt = do
  void (symbol "var")
  name <- ident
  ty <- optional (symbol ":" *> parseType)
  val <- optional (symbol "=" *> pExpr)
  semi
  pure (SVar name ty val)

-- | Parse ref declaration
pRefStmt :: Parser Stmt
pRefStmt = do
  void (symbol "ref")
  name <- ident
  void (symbol "=")
  path <- pRefPath
  semi
  pure (SRef name path)

-- | Parse assignment statement
pAssignStmt :: Parser Stmt
pAssignStmt = do
  path <- pRefPath
  op <- pAssignOp
  val <- pExpr
  semi
  pure (SAssign path op val)

-- | Parse await statement
pAwaitStmt :: Parser Stmt
pAwaitStmt = try withVar <|> withoutVar
  where
    withVar = do
      void (symbol "var")
      name <- ident
      void (symbol "=")
      void (symbol "await")
      expr <- pExpr
      semi
      pure (SAwait (Just name) expr)
    
    withoutVar = do
      void (symbol "await")
      expr <- pExpr
      semi
      pure (SAwait Nothing expr)

-- | Parse if statement
pIfStmt :: Parser Stmt
pIfStmt = do
  void (symbol "if")
  cond <- parens pExpr
  thenStmts <- braces (many pStmt)
  elseStmts <- optional (symbol "else" *> braces (many pStmt))
  pure (SIf cond thenStmts elseStmts)

-- | Parse for statement
pForStmt :: Parser Stmt
pForStmt = do
  void (symbol "for")
  void (symbol "(")
  init' <- optional (try pStmt)
  cond <- optional pExpr
  semi
  incr <- optional pExpr
  void (symbol ")")
  body <- braces (many pStmt)
  pure (SFor init' cond incr body)

-- | Parse while statement
pWhileStmt :: Parser Stmt
pWhileStmt = do
  void (symbol "while")
  cond <- parens pExpr
  body <- braces (many pStmt)
  pure (SWhile cond body)

-- | Parse switch statement
pSwitchStmt :: Parser Stmt
pSwitchStmt = do
  void (symbol "switch")
  expr <- parens pExpr
  void (symbol "{")
  cases <- many pSwitchCase
  def <- optional (symbol "default" *> symbol ":" *> many pStmt)
  void (symbol "}")
  pure (SSwitch expr cases def)

-- | Parse switch case
pSwitchCase :: Parser SwitchCase
pSwitchCase = do
  void (symbol "case")
  val <- pExpr
  void (symbol ":")
  stmts <- many pStmt
  pure (SwitchCase val stmts)

-- | Parse return statement
pReturnStmt :: Parser Stmt
pReturnStmt = do
  void (symbol "return")
  expr <- optional pExpr
  semi
  pure (SReturn expr)

-- | Parse break statement
pBreakStmt :: Parser Stmt
pBreakStmt = SBreak <$ symbol "break" <* semi

-- | Parse continue statement
pContinueStmt :: Parser Stmt
pContinueStmt = SContinue <$ symbol "continue" <* semi

-- | Parse throw statement
pThrowStmt :: Parser Stmt
pThrowStmt = do
  void (symbol "throw")
  expr <- pExpr
  semi
  pure (SThrow expr)

-- | Parse try statement
pTryStmt :: Parser Stmt
pTryStmt = do
  void (symbol "try")
  tryBody <- braces (many pStmt)
  catches <- many pCatchClause
  finally' <- optional (symbol "finally" *> braces (many pStmt))
  pure (STry tryBody catches finally')

-- | Parse catch clause
pCatchClause :: Parser (Text, Text, [L Stmt])
pCatchClause = do
  void (symbol "catch")
  void (symbol "(")
  ty <- ident
  name <- ident
  void (symbol ")")
  body <- braces (many pStmt)
  pure (ty, name, body)

-- | Parse reference path
pRefPath :: Parser RefPath
pRefPath = do
  root <- pRefRoot
  parts <- many pRefPart
  pure (RefPath root parts)

-- | Parse reference root
pRefRoot :: Parser RefRoot
pRefRoot = choice
  [ RThis <$ symbol "this"
  , RName <$> ident
  ]

-- | Parse reference part
pRefPart :: Parser RefPart
pRefPart = choice
  [ symbol "." *> (PField <$> ident)
  , PIndex <$> brackets pExpr
  ]

-- | Parse assignment operator
pAssignOp :: Parser AssignOp
pAssignOp = choice
  [ APlusEq  <$ symbol "+="
  , AMinusEq <$ symbol "-="
  , AMulEq   <$ symbol "*="
  , ADivEq   <$ symbol "/="
  , AModEq   <$ symbol "%="
  , AEq      <$ symbol "="
  ]

-- * Program parser (for imp quasiquoter)

-- | Parse a program (optional capability header + statements)
parseProgram :: String -> Text -> Either ImpParseError Program
parseProgram filename input = runParser pProgram filename input

pProgram :: Parser Program
pProgram = do
  sc
  hdr <- optional (try pCapHeader)
  stmts <- many pStmt
  eof
  pure (Program hdr stmts)

-- | Parse capability header
pCapHeader :: Parser (L CapHeader)
pCapHeader = withSpan $ do
  void (symbol "capabilities")
  caps <- parens (ident `sepBy` comma)
  semi
  pure (CapHeader caps)

-- * Module parser (for impModule quasiquoter)

-- | Parse a module
parseModule :: String -> Text -> Either ImpParseError Module
parseModule filename input = runParser pModule filename input

pModule :: Parser Module
pModule = do
  sc
  items <- many pTopItem
  eof
  pure (Module items)

-- | Parse a top-level item
pTopItem :: Parser (L TopItem)
pTopItem = withSpan $ choice
  [ THsDec <$> hsDecBlock
  , TCapability <$> pCapDecl
  , TType <$> pTypeDecl
  , TNewtype <$> pNewtypeDecl
  , TEnum <$> pEnumDecl
  , try (TFn <$> pFnDecl)
  , TProc <$> pProcDecl
  , TOperators <$> pOpBlock
  ]

-- | Parse type declaration
pTypeDecl :: Parser TypeDecl
pTypeDecl = do
  void (symbol "type")
  name <- ident
  params <- option [] (angles (ident `sepBy` comma))
  deriving' <- option [] pDeriving
  fields <- braces (many pFieldDecl)
  pure (TypeDecl name params fields deriving')

-- | Parse field declaration
pFieldDecl :: Parser FieldDecl
pFieldDecl = do
  name <- ident
  void (symbol ":")
  ty <- parseType
  semi
  pure (FieldDecl name ty)

-- | Parse newtype declaration
pNewtypeDecl :: Parser NewtypeDecl
pNewtypeDecl = do
  void (symbol "newtype")
  name <- ident
  params <- option [] (angles (ident `sepBy` comma))
  void (symbol "=")
  ty <- parseType
  deriving' <- option [] pDeriving
  semi
  pure (NewtypeDecl name params ty deriving')

-- | Parse enum declaration
pEnumDecl :: Parser EnumDecl
pEnumDecl = do
  void (symbol "enum")
  name <- ident
  params <- option [] (angles (ident `sepBy` comma))
  deriving' <- option [] pDeriving
  void (symbol "{")
  variants <- pEnumVariant `sepBy` comma
  void (symbol "}")
  pure (EnumDecl name params variants deriving')

-- | Parse enum variant
pEnumVariant :: Parser EnumVariant
pEnumVariant = do
  name <- ident
  fields <- option [] (parens (parseType `sepBy` comma))
  pure (EnumVariant name fields)

-- | Parse deriving clause
pDeriving :: Parser [Text]
pDeriving = do
  void (symbol "deriving")
  parens (ident `sepBy` comma)

-- | Parse capability declaration
pCapDecl :: Parser CapDecl
pCapDecl = do
  void (symbol "capability")
  name <- ident
  super <- option [] pRequires
  methods <- braces (many pCapMethod)
  pure (CapDecl name super methods)

-- | Parse capability method
pCapMethod :: Parser CapMethod
pCapMethod = do
  name <- ident
  params <- parens (pParam `sepBy` comma)
  void (symbol ":")
  ret <- parseType
  semi
  pure (CapMethod name params ret)

-- | Parse requires clause
pRequires :: Parser [Constraint]
pRequires = do
  void (symbol "requires")
  parens (pConstraint `sepBy` comma)

-- | Parse constraint
pConstraint :: Parser Constraint
pConstraint = do
  name <- qualIdent
  args <- option [] (angles (parseType `sepBy` comma))
  pure (Constraint name args)

-- | Parse operator block
pOpBlock :: Parser OpBlock
pOpBlock = do
  void (symbol "operators")
  mappings <- braces (many pOpMapping)
  pure (OpBlock mappings)

-- | Parse operator mapping
pOpMapping :: Parser (BinOp, [Text])
pOpMapping = do
  op <- pBinOp
  void (symbol "=")
  name <- qualIdent
  semi
  pure (op, name)

-- | Parse binary operator symbol
pBinOp :: Parser BinOp
pBinOp = choice
  [ OpAdd    <$ symbol "+"
  , OpSub    <$ symbol "-"
  , OpMul    <$ symbol "*"
  , OpDiv    <$ symbol "/"
  , OpMod    <$ symbol "%"
  , OpEq     <$ symbol "=="
  , OpNeq    <$ symbol "!="
  , OpLte    <$ symbol "<="
  , OpLt     <$ symbol "<"
  , OpGte    <$ symbol ">="
  , OpGt     <$ symbol ">"
  , OpAnd    <$ symbol "&&"
  , OpOr     <$ symbol "||"
  , OpBitAnd <$ symbol "&"
  , OpBitOr  <$ symbol "|"
  , OpBitXor <$ symbol "^"
  , OpShiftL <$ symbol "<<"
  , OpShiftR <$ symbol ">>"
  ]

-- | Parse function declaration
pFnDecl :: Parser FnDecl
pFnDecl = do
  void (symbol "fn")
  name <- ident
  params <- parens (pParam `sepBy` comma)
  ret <- optional (symbol ":" *> parseType)
  requires' <- option [] pRequires
  body <- braces (many pStmt)
  pure (FnDecl name params ret requires' body)

-- | Parse procedure declaration
pProcDecl :: Parser ProcDecl
pProcDecl = do
  void (symbol "proc")
  name <- ident
  params <- parens (pParam `sepBy` comma)
  ret <- optional (symbol ":" *> parseType)
  requires' <- option [] pRequires
  body <- braces (many pStmt)
  pure (ProcDecl name params ret requires' body)

-- | Parse parameter
pParam :: Parser Param
pParam = do
  name <- ident
  void (symbol ":")
  ty <- parseType
  pure (Param name ty)

-- * Expression parser for standalone testing
parseExpr :: String -> Text -> Either ImpParseError (L Expr)
parseExpr filename input = runParser (sc *> pExpr <* eof) filename input
