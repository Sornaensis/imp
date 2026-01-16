module Imp.CodegenGolden
  ( renderDecSummary
  , expUsesNameBase
  ) where

import Data.List (intercalate)
import Language.Haskell.TH (Body(..), Con(..), Dec(..), Exp(..), Match(..), Pat(..), Pred, Stmt(..), TyVarBndr(..), Type(..), nameBase)

renderDecSummary :: [Dec] -> String
renderDecSummary decs = unlines (concatMap renderDec decs)

renderDec :: Dec -> [String]
renderDec dec = case dec of
  DataD _ name _ _ cons _ ->
    ["data " <> nameBase name <> " { " <> renderCons cons <> " }"]
  ClassD _ name (mVar:_) _ sigs ->
    ["class " <> nameBase name <> " " <> renderTyVarName mVar <> " { " <> intercalate ", " (map renderClassSig sigs) <> " }"]
  SigD name ty ->
    ["sig " <> nameBase name <> " :: " <> renderType ty]
  _ -> []
  where
    renderCons :: [Con] -> String
    renderCons [RecC _ fields] = intercalate ", "
      [ nameBase fname <> " :: " <> renderType fty
      | (fname, _, fty) <- fields
      ]
    renderCons _ = ""

renderClassSig :: Dec -> String
renderClassSig (SigD name ty) = nameBase name <> " :: " <> renderType ty
renderClassSig _ = ""

renderType :: Type -> String
renderType ty = case ty of
  ForallT vars preds t ->
    "forall " <> unwords (map renderTyVar vars) <> ". " <> renderContext preds <> renderType t
  AppT (AppT ArrowT a) b ->
    renderTypeAtom a <> " -> " <> renderType b
  AppT a b -> renderType a <> " " <> renderTypeAtom b
  ConT name -> nameBase name
  VarT name -> nameBase name
  TupleT 0 -> "()"
  ListT -> "[]"
  _ -> "<type>"
  where
    renderTyVar (PlainTV name _) = normalizeTyVar (nameBase name)
    renderTyVar (KindedTV name _ _) = normalizeTyVar (nameBase name)
    renderTyVar _ = "_"
    normalizeTyVar :: String -> String
    normalizeTyVar "_" = "m"
    normalizeTyVar other = other

renderTyVarName :: TyVarBndr flag -> String
renderTyVarName tyVar = case tyVar of
  PlainTV name _ -> nameBase name
  KindedTV name _ _ -> nameBase name

renderTypeAtom :: Type -> String
renderTypeAtom t@(AppT (AppT ArrowT _) _) = "(" <> renderType t <> ")"
renderTypeAtom t@(ForallT _ _ _) = "(" <> renderType t <> ")"
renderTypeAtom t = renderType t

renderContext :: [Pred] -> String
renderContext [] = ""
renderContext preds = "(" <> intercalate ", " (map renderPred preds) <> ") => "

renderPred :: Pred -> String
renderPred pred' = case pred' of
  AppT (ConT name) (VarT var) -> nameBase name <> " " <> nameBase var
  _ -> renderType pred'

expUsesNameBase :: String -> Exp -> Bool
expUsesNameBase target exp' = case exp' of
  VarE name -> nameBase name == target
  AppE f x -> expUsesNameBase target f || expUsesNameBase target x
  InfixE a op b -> any (expUsesNameBase target) (maybeToList a <> [op] <> maybeToList b)
  LamE _ body -> expUsesNameBase target body
  LetE decs body -> any decUsesName decs || expUsesNameBase target body
  CaseE scrut matches -> expUsesNameBase target scrut || any matchUsesName matches
  DoE _ stmts -> any stmtUsesName stmts
  CompE stmts -> any stmtUsesName stmts
  ListE xs -> any (expUsesNameBase target) xs
  TupE xs -> any (maybe False (expUsesNameBase target)) xs
  UnboxedTupE xs -> any (maybe False (expUsesNameBase target)) xs
  CondE a b c -> any (expUsesNameBase target) [a, b, c]
  SigE e _ -> expUsesNameBase target e
  ParensE e -> expUsesNameBase target e
  _ -> False
  where
    maybeToList = maybe [] pure
    decUsesName (ValD _ (NormalB e) _) = expUsesNameBase target e
    decUsesName _ = False
    matchUsesName (Match _ (NormalB e) _) = expUsesNameBase target e
    matchUsesName _ = False
    stmtUsesName stmt = case stmt of
      BindS _ e -> expUsesNameBase target e
      LetS decs -> any decUsesName decs
      NoBindS e -> expUsesNameBase target e
      ParS stmtss -> any (any stmtUsesName) stmtss
