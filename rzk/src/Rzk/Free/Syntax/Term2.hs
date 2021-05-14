{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Rzk.Free.Syntax.Term2 where

import           Bound.Name
import           Bound.Scope
import           Bound.Var
import           Control.Monad.State
import           Data.Bifunctor.TH
import           Data.Coerce
import           Data.List                              (intercalate)
import           Data.Void

import           Rzk.Free.Syntax.FreeScoped
import           Rzk.Free.Syntax.FreeScoped.Unification




-- data ExprF var expr
--   = LambdaF var expr
--   | AppF expr expr
--   | VarF var
--
-- zipMatch
--   :: ExprF v e
--   -> ExprF v e
--   -> Maybe (ExprF v (Either e (e, e)))
-- zipMatch e1 e2 =
--   case (e1, e2) of
--     (AppF f1 x1, AppF f2 x2) ->
--       Just (AppF (Right (f1, f2))
--                  (Right (x1, x2)))
--
-- lambda x. (lambda y a. y a) (lambda z. z)
--
--
-- lambda x. T1
-- lambda y. T2
--
-- T1[fresh/x]
-- T2[fresh/y]
--
--
--
-- lambda x. x 2 3
-- lambda x. M
--
-- fresh1 2 3
-- M
--
-- M := fresh1 2 3

-- M |-> lambda y1 ... yn. H T1 .. Tk

-- lambda x1 x2 ... xn. @ t1 t2 .. tk
-- lambda x1 x2 ... xm. M e1 e2 .. el
--
-- M = lambda y1 ... yl. @ (M1 y1 ... yl) .. (Mk y1 ... yl)
-- M = lambda y1 ... yl. yi (M1' y1 ... yl) .. (Mk1' y1 .. yl)
--
--
-- type Expr var = Fix (Expr var)
















data ExprF scope term
  = NumberF Int
  | AddF term term
  | FunF Int scope
  | LetF term scope
  | FunCallF term [term]
  deriving (Eq, Show, Functor, Foldable, Traversable)

toHeadF :: ExprF scope term -> Maybe (ExprF scope (Either term term))
toHeadF = \case
  NumberF{} -> Nothing
  e@AddF{} -> Just (Right <$> e)
  LetF{} -> Nothing
  FunF{} -> Nothing
  FunCallF f args -> Just (FunCallF (Left f) (Right <$> args))

fromHeadF :: ExprF scope (Either term term) -> ExprF scope term
fromHeadF = fmap g
  where
    g = \case
      Left  x -> x
      Right y -> y

appF :: term -> [term] -> (ExprF scope term, [term])
appF f args = (FunCallF f args, [])

abstractF :: Int -> scope -> ExprF scope term
abstractF n body = FunF n body

deriveBifunctor     ''ExprF
deriveBifoldable    ''ExprF
deriveBitraversable ''ExprF

zipMatchExprF
  :: ExprF s t -> ExprF s t -> Maybe (ExprF (Either s (s, s)) (Either t (t, t)))
zipMatchExprF t1 t2 =
  case (t1, t2) of
    (NumberF n, NumberF m)
      | n == m    -> Just (NumberF n)
      | otherwise -> Nothing
    (AddF x1 y1, AddF x2 y2) -> Just (AddF (Right (x1, x2)) (Right (y1, y2)))
    (FunF n1 body1, FunF n2 body2)
      | n1 == n2 -> Just (FunF n1 (Right (body1, body2)))
      | otherwise -> Nothing
    (LetF e1 next1, LetF e2 next2)
      -> Just (LetF (Right (e1, e2)) (Right (next1, next2)))
    (FunCallF f1 args1, FunCallF f2 args2)
      -> FunCallF (Right (f1, f2)) <$> zipMatchList args1 args2
    _ -> Nothing
  where
    zipMatchList [] []         = Just []
    zipMatchList (x:xs) (y:ys) = (Right (x, y) :) <$> zipMatchList xs ys
    zipMatchList _ _           = Nothing

newtype UExpr a = UExpr { unwrapUExpr :: Expr (UVar (Name String Int) a Int) }

instance Show (UExpr String) where
  show = show . fmap fromUVar . unwrapUExpr
    where
      fromUVar (UFreeVar x)    = x
      fromUVar (UBoundVar v _) = "?[" <> show v <> "]"
      fromUVar (UMetaVar v)    = "?" <> show v

unifyExpr :: Eq a => UExpr a -> UExpr a -> Maybe ([(Int, UExpr a)], [(UExpr a, UExpr a)])
unifyExpr e1 e2 = fmap coerce $
  driver (coerce reduce) zipMatchExprF (coerce peel) (coerce FunCall) mkLams
    (coerce e1, coerce e2)
  where
    peel (FunCall f args) = (f, args)
    peel e                = (e, [])

    mkLams n body = coerce (Fun n body')
      where
        UnScopedExpr body' = mapBound (Name "") body

pattern Coerce :: Coercible a b => a -> b
pattern Coerce e <- (coerce -> e)
  where Coerce e = coerce e

newtype Expr a = Expr (FreeScoped (Name String Int) ExprF a)
  deriving (Functor, Applicative, Monad)

newtype ScopedExpr a = ScopedExpr (Scope (Name String Int) Expr a)

newtype S f a = S (f (Maybe (f a)))

newtype F a = F (Maybe a)

newtype SF a = SF (S F a)

bad :: SF a -> S Maybe a
bad = coerce

fromScopedExpr
  :: ScopedExpr a -> Scope (Name String Int) (FreeScoped (Name String Int) ExprF) a
fromScopedExpr (ScopedExpr (Scope s)) = coerce (fmap (fmap coerce) s) -- why not coerce?

toScopedExpr
  :: Scope (Name String Int) (FreeScoped (Name String Int) ExprF) a -> ScopedExpr a
toScopedExpr (Scope s) = coerce (fmap (fmap coerce) s)  -- why not coerce?


pattern UnScopedExpr
  :: ScopedExpr a
  -> Scope (Name String Int) (FreeScoped (Name String Int) ExprF) a
pattern UnScopedExpr e <- (toScopedExpr -> e)
  where UnScopedExpr e = fromScopedExpr e

instantiateExpr :: (Int -> Expr a) -> ScopedExpr a -> Expr a
instantiateExpr f scope = Expr (instantiate (\(Name _ i) -> coerce (f i)) (UnScopedExpr scope))

pattern Variable :: a -> Expr a
pattern Variable x = Coerce (PureScoped x)

pattern Number :: Int -> Expr a
pattern Number n = Coerce (FreeScoped (NumberF n))

pattern Add :: Expr a -> Expr a -> Expr a
pattern Add x y = Coerce (FreeScoped (AddF (Coerce x) (Coerce y)))

pattern Let :: Expr a -> ScopedExpr a -> Expr a
pattern Let e scope = Coerce (FreeScoped (LetF (Coerce e) (UnScopedExpr scope)))

pattern FunCall :: Expr a -> [Expr a] -> Expr a
pattern FunCall f args = Coerce (FreeScoped (FunCallF (Coerce f) (Coerce args)))

pattern Fun :: Int -> ScopedExpr a -> Expr a
pattern Fun n body = Coerce (FreeScoped (FunF n (UnScopedExpr body)))

{-# COMPLETE Variable, Number, Add, Let, FunCall #-}

instance Num (Expr a) where
  fromInteger = Number . fromInteger
  (+) = Add

instance Show (Expr String) where
  show = ppExpr (map pure ['a'..'z'])

reduce :: Expr a -> Expr a
reduce = \case
  Variable x -> Variable x
  Number n -> Number n
  Add (Number n) (Number m) -> Number (n + m)
  Add x y -> Add (reduce x) (reduce y)
  Fun n body -> Fun n body
  Let expr next ->
    reduce (instantiateExpr (\0 -> expr) next)
  FunCall (Fun n scope) args ->
    let args' = map reduce args
      in reduce (instantiateExpr (args' !!) scope)
  FunCall f args -> FunCall (reduce f) (map reduce args)

evalExpr :: Expr Void -> Int
evalExpr = \case
  Variable x -> absurd x
  Number n -> n
  Add x y -> evalExpr x + evalExpr y
  Let expr next ->
    evalExpr (instantiateExpr (\0 -> expr) next)
  FunCall (Fun n scope) args ->
    let args' = map (Number . evalExpr) args
      in evalExpr (instantiateExpr (args' !!) scope)
  FunCall _ _ -> error "non-function application"

ppExprScope :: [String] -> [String] -> ScopedExpr String -> String
ppExprScope boundVars freshVars
  = ppExpr freshVars . instantiateExpr boundVar
  where
    boundVar i = Variable (boundVars !! i)

ppExpr :: [String] -> Expr String -> String
ppExpr vars = \case
  Variable x -> x
  Number n -> show n
  Add x y -> ppExprParens vars x <> " + " <> ppExprParens vars y
  Fun n scope ->
    let (ys, zs) = splitAt n vars
     in "fun (" <> intercalate ", " ys <> ") => " <> ppExprScope ys zs scope

  Let (Fun n scope) next ->
    let x:xs = vars
        (ys, zs) = splitAt n xs
     in x <> "(" <> intercalate ", " ys <> ") :=\n" <> indent (ppExprScope ys zs scope) <> "\n"
        <> ppExprScope [x] xs next

  Let expr next ->
    let x:xs = vars
     in x <> " := " <> ppExpr xs expr <> "\n" <> ppExprScope [x] xs next

  FunCall f args ->
    ppExprParens vars f <> "(" <> intercalate ", " (map (ppExpr vars) args) <> ")"

ppExprParens :: [String] -> Expr String -> String
ppExprParens vars = \case
  e@Variable{} -> ppExpr vars e
  e@Number{} -> ppExpr vars e
  e@Add{} -> parens (ppExpr vars e)
  e@Let{} -> parens (ppExpr vars e)
  e@FunCall{} -> ppExpr vars e

parens :: String -> String
parens s = "(" <> s <> ")"

indent :: String -> String
indent = unlines . map ("  " <> ) . lines

-- |
-- >>> putStrLn $ ppExpr (map pure ['a'..'z']) exampleExpr1
-- a(b, c) :=
--   b + c
-- <BLANK>
-- a(2, 3)
--
-- >>> evalExpr (undefined <$ exampleExpr1)
-- 5
exampleExpr1 :: Expr String
exampleExpr1 =
  Let (Fun 2 (ScopedExpr (Scope (Add (pure (B (Name "x" 0))) (pure (B (Name "y" 1)))))))
    (ScopedExpr (Scope (FunCall (pure (B (Name "f" 0))) [2, 3])))

exampleExprUnify :: IO ()
exampleExprUnify = do
  print e1
  print e2
  print (UExpr (reduce (unwrapUExpr e1)))
  print (UExpr (reduce (unwrapUExpr e2)))
  print e12
  print (unifyExpr e2 e1)
  where
    e1, e2 :: UExpr String
    e1 = UExpr (FunCall 2 [3])
    e2 = UExpr (FunCall (Variable (UMetaVar 0)) [3])

    e12 :: [[(UExpr String, UExpr String)]]
    e12 = flip evalStateT initBindState . runAssocBindT
      $ fmap coerce <$> repeatedlySimplify (coerce reduce) zipMatchExprF (coerce peel) [(coerce e1, coerce e2)]


    peel (FunCall f args) = (f, args)
    peel e                = (e, [])
