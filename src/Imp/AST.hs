{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}

-- | AST types for the imperative DSL with source span tracking
module Imp.AST where

import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.Megaparsec (SourcePos)

-- * Source Spans

-- | Represents a source location span
data Span = Span
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Show, Eq, Data, Typeable)

-- | Located node wrapper - carries span information
data L a = L
  { lSpan :: Span
  , lVal  :: a
  } deriving (Show, Eq, Data, Typeable, Functor, Foldable, Traversable)

-- | Merge two spans into one encompassing both
mergeSpan :: Span -> Span -> Span
mergeSpan (Span s _) (Span _ e) = Span s e

-- * Type System

-- | Type annotations in the DSL
data Type
  = TPrim PrimType
  | TUser [Text]                    -- qualified or unqualified user type
  | TOption (L Type)                -- option<T>
  | TList (L Type)                  -- list<T>
  | TMap (L Type) (L Type)          -- map<K,V>
  | TGeneric Text [L Type]          -- generic type application
  deriving (Show, Eq, Data, Typeable)

-- | Built-in primitive types
data PrimType
  = PBool
  | PChar
  | PString
  | PText
  | PByte
  | PShort
  | PInt
  | PLong
  | PFloat
  | PDouble
  | PBytes
  | PUnit
  | PUuid
  deriving (Show, Eq, Data, Typeable)

-- * Expressions

-- | Expressions in the DSL
data Expr
  = EVar Text                       -- variable reference
  | EThis                           -- this reference
  | ELit Lit                        -- literal value
  | ECall (L Expr) [L Expr]         -- function/method call
  | EMember (L Expr) Text           -- member access: x.field
  | ERecordUpdate (L Expr) [(Text, L Expr)] -- record update: x { field: expr }
  | EIndex (L Expr) (L Expr)        -- index access: x[i]
  | EBinOp BinOp (L Expr) (L Expr)  -- binary operation
  | EUnOp UnOp (L Expr)             -- unary operation
  | ECapCall Text Text [L Expr]     -- capability call: capabilities.cap.method(args)
  | EHs Text                        -- raw Haskell expression: hs{...}
  | EHsM Text                       -- raw Haskell monadic expression: hsM{...}
  | ENew (L Type) [L Expr]          -- constructor call: new Type(args)
  deriving (Show, Eq, Data, Typeable)

-- | Literal values
data Lit
  = LInt Integer
  | LDouble Double
  | LText Text
  | LBool Bool
  | LNull
  | LTagged Text Lit
  deriving (Show, Eq, Data, Typeable)

-- | Binary operators
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
  | OpAnd | OpOr
  | OpBitAnd | OpBitOr | OpBitXor
  | OpShiftL | OpShiftR
  deriving (Show, Eq, Ord, Data, Typeable)

-- | Unary operators
data UnOp
  = OpNot
  | OpNeg
  | OpBitNot
  deriving (Show, Eq, Data, Typeable)

-- * Statements

-- | Statements in the DSL
data Stmt
  = SVar Text (Maybe (L Type)) (Maybe (L Expr))  -- var x: T = expr;
  | SRef Text RefPath                             -- ref x = this.field;
  | SAssign RefPath AssignOp (L Expr)             -- x = expr; or x += expr;
  | SAwait (Maybe Text) (L Expr)                  -- await expr; or var x = await expr;
  | SExpr (L Expr)                                -- expr;
  | SIf (L Expr) [L Stmt] (Maybe [L Stmt])        -- if (cond) {...} else {...}
  | SFor (Maybe (L Stmt)) (Maybe (L Expr)) (Maybe (L Expr)) [L Stmt]  -- for (init; cond; incr) {...}
  | SWhile (L Expr) [L Stmt]                      -- while (cond) {...}
  | SSwitch (L Expr) [SwitchCase] (Maybe [L Stmt]) -- switch (expr) { case...; default: ... }
  | SReturn (Maybe (L Expr))                      -- return expr?;
  | SBreak                                        -- break;
  | SContinue                                     -- continue;
  | SThrow (L Expr)                               -- throw expr;
  | STry [L Stmt] [(Text, Text, [L Stmt])] (Maybe [L Stmt]) -- try {...} catch (T x) {...} finally {...}
  | SBlock [L Stmt]                               -- { ... }
  deriving (Show, Eq, Data, Typeable)

-- | Switch case
data SwitchCase = SwitchCase (L Expr) [L Stmt]
  deriving (Show, Eq, Data, Typeable)

-- | Assignment operators
data AssignOp
  = AEq       -- =
  | APlusEq   -- +=
  | AMinusEq  -- -=
  | AMulEq    -- *=
  | ADivEq    -- /=
  | AModEq    -- %=
  deriving (Show, Eq, Data, Typeable)

-- | Reference paths (for ref and assignment)
data RefPath = RefPath RefRoot [RefPart]
  deriving (Show, Eq, Data, Typeable)

-- | Reference root
data RefRoot
  = RThis       -- this
  | RName Text  -- variable name
  deriving (Show, Eq, Data, Typeable)

-- | Reference path components
data RefPart
  = PField Text      -- .field
  | PIndex (L Expr)  -- [expr]
  deriving (Show, Eq, Data, Typeable)

-- * Module-level declarations

-- | Top-level module items
data TopItem
  = TType TypeDecl
  | TNewtype NewtypeDecl
  | TEnum EnumDecl
  | TCapability CapDecl
  | TOperators OpBlock
  | TFn FnDecl
  | TProc ProcDecl
  | THsDec Text                     -- raw Haskell: #{...}
  deriving (Show, Eq, Data, Typeable)

-- | Type declaration
data TypeDecl = TypeDecl
  { tdName    :: Text
  , tdParams  :: [Text]
  , tdFields  :: [FieldDecl]
  , tdDeriving :: [Text]
  } deriving (Show, Eq, Data, Typeable)

-- | Field declaration
data FieldDecl = FieldDecl
  { fdName :: Text
  , fdType :: L Type
  } deriving (Show, Eq, Data, Typeable)

-- | Newtype declaration
data NewtypeDecl = NewtypeDecl
  { ntdName    :: Text
  , ntdParams  :: [Text]
  , ntdType    :: L Type
  , ntdDeriving :: [Text]
  } deriving (Show, Eq, Data, Typeable)

-- | Enum declaration
data EnumDecl = EnumDecl
  { edName    :: Text
  , edParams  :: [Text]
  , edVariants :: [EnumVariant]
  , edDeriving :: [Text]
  } deriving (Show, Eq, Data, Typeable)

-- | Enum variant
data EnumVariant = EnumVariant
  { evName   :: Text
  , evFields :: [L Type]
  } deriving (Show, Eq, Data, Typeable)

-- | Capability declaration
data CapDecl = CapDecl
  { cdName    :: Text
  , cdSuper   :: [Constraint]
  , cdMethods :: [CapMethod]
  } deriving (Show, Eq, Data, Typeable)

-- | Capability method
data CapMethod = CapMethod
  { cmName   :: Text
  , cmParams :: [Param]
  , cmReturn :: L Type
  } deriving (Show, Eq, Data, Typeable)

-- | Type constraint
data Constraint = Constraint
  { cName :: [Text]
  , cArgs :: [L Type]
  } deriving (Show, Eq, Data, Typeable)

-- | Operator mapping block
data OpBlock = OpBlock
  { obMappings :: [(BinOp, [Text])]
  } deriving (Show, Eq, Data, Typeable)

-- | Function declaration (pure)
data FnDecl = FnDecl
  { fnName     :: Text
  , fnParams   :: [Param]
  , fnReturn   :: Maybe (L Type)
  , fnRequires :: [Constraint]
  , fnBody     :: [L Stmt]
  } deriving (Show, Eq, Data, Typeable)

-- | Procedure declaration (effectful)
data ProcDecl = ProcDecl
  { procName     :: Text
  , procParams   :: [Param]
  , procReturn   :: Maybe (L Type)
  , procRequires :: [Constraint]
  , procBody     :: [L Stmt]
  } deriving (Show, Eq, Data, Typeable)

-- | Parameter
data Param = Param
  { pName :: Text
  , pType :: L Type
  } deriving (Show, Eq, Data, Typeable)

-- * Capability headers in blocks

-- | Capability header at the start of a block
data CapHeader = CapHeader [Text]
  deriving (Show, Eq, Data, Typeable)

-- | A program is a capability header plus statements
data Program = Program
  { progCapHeader :: Maybe (L CapHeader)
  , progStmts     :: [L Stmt]
  } deriving (Show, Eq, Data, Typeable)

-- | A module is a list of top-level items
data Module = Module
  { modItems :: [L TopItem]
  } deriving (Show, Eq, Data, Typeable)
