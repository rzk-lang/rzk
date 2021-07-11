{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Rzk.Free.Syntax.Example.STLC where

-- import           Debug.Trace
import           Unsafe.Coerce

import qualified Bound.Scope                             as Scope
import qualified Bound.Var                               as Bound
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Char                               (chr, ord)
import           Data.Maybe                              (fromMaybe)
import           Data.String                             (IsString (..))
import           Data.Text.Prettyprint.Doc               as Doc

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.FreeScoped
import           Rzk.Free.Syntax.FreeScoped.TypeCheck    (TypeCheck, TypeError,
                                                          TypeInfo, assignType,
                                                          clarifyTypedTerm,
                                                          freshTypeMetaVar,
                                                          getTypeInfo, nonDep,
                                                          shouldHaveType,
                                                          typeOf,
                                                          typeOfScopedWith,
                                                          typecheckDist,
                                                          typecheckInScope,
                                                          unifyWithExpected,
                                                          untyped,
                                                          untypedScoped)
import qualified Rzk.Free.Syntax.FreeScoped.TypeCheck    as TypeCheck
import           Rzk.Free.Syntax.FreeScoped.Unification  (UVar (..))
import           Rzk.Free.Syntax.FreeScoped.Unification2 (Unifiable (..))
import qualified Rzk.Syntax.Var                          as Rzk

trace :: String -> a -> a
trace = const id

traceShow :: Show b => b -> a -> a
traceShow = trace . show

-- | Generating functor for terms in Martin-Loef Type Theory.
data TermF scope term
  = UniverseF

  | FunF term term
  | LamF (Maybe term) scope
  | AppF term term

  | UnitTypeF
  | UnitF

  | LetF term scope

--  | TypeAsc term term
--  | ...

--  | BoolF
--  | TrueF
--  | FalseF
--  | IfF
  deriving (Show, Functor, Foldable, Traversable)

type Term b = TypeCheck.Term TermF b
type TermInScope b a = TypeCheck.TermInScope TermF b a
type ScopedTerm b = TypeCheck.ScopedTerm TermF b

type TypedTermF = TypeCheck.TypedF TermF

type TypedTerm b = TypeCheck.TypedTerm TermF b
type TypedTermInScope b a = TypeCheck.TypedTermInScope TermF b a
type ScopedTypedTerm b = TypeCheck.ScopedTypedTerm TermF b

type UTypedTerm b a v = TypeCheck.UTypedTerm TermF b a v
type UTypedTermInScope b a v = TypeCheck.UTypedTermInScope TermF b a v
type UScopedTypedTerm b a v = TypeCheck.UScopedTypedTerm TermF b a v

type Term' = Term Rzk.Var Rzk.Var
type TermInScope' = TermInScope Rzk.Var Rzk.Var
type ScopedTerm' = ScopedTerm Rzk.Var Rzk.Var

type TypedTerm' = TypedTerm Rzk.Var Rzk.Var
type TypedTermInScope' = TypedTermInScope Rzk.Var Rzk.Var
type ScopedTypedTerm' = ScopedTypedTerm Rzk.Var Rzk.Var

type UTypedTerm' = UTypedTerm Rzk.Var Rzk.Var Rzk.Var
type UTypedTermInScope' = UTypedTermInScope Rzk.Var Rzk.Var Rzk.Var
type UScopedTypedTerm' = UScopedTypedTerm Rzk.Var Rzk.Var Rzk.Var

type InScope' = Bound.Var (Name Rzk.Var ())

type UTypedTerm'1 = UTypedTerm Rzk.Var (InScope' Rzk.Var) Rzk.Var
type UTypedTerm'2 = UTypedTerm Rzk.Var (InScope' (InScope' Rzk.Var)) Rzk.Var

type TypeInfo'2 = TypeInfo Rzk.Var UTypedTerm'2 (InScope' (InScope' Rzk.Var))

-- | A variable.
pattern Var :: a -> Term b a
pattern Var x = PureScoped x

-- | Universe type \(\mathcal{U}_i\)
pattern Universe :: Term b a
pattern Universe = FreeScoped UniverseF

pattern Unit :: Term b a
pattern Unit = FreeScoped UnitF

pattern UnitType :: Term b a
pattern UnitType = FreeScoped UnitTypeF

pattern Let :: Term b a -> ScopedTerm b a -> Term b a
pattern Let u t = FreeScoped (LetF u t)

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern Fun :: Term b a -> Term b a -> Term b a
pattern Fun a b = FreeScoped (FunF a b)

-- | A \(\lambda\)-abstraction.
pattern Lam :: Maybe (Term b a) -> ScopedTerm b a -> Term b a
pattern Lam ty body = FreeScoped (LamF ty body)

-- | An application of one term to another.
pattern App :: Term b a -> Term b a -> Term b a
pattern App t1 t2 = FreeScoped (AppF t1 t2)

{-# COMPLETE Var, Universe, UnitType, Unit, Let, Fun, Lam, App #-}

-- | A variable.
pattern VarT :: a -> TypedTerm b a
pattern VarT x = PureScoped x

-- | Universe type \(\mathcal{U}_i\)
pattern UniverseT :: TypedTerm b a -> TypedTerm b a
pattern UniverseT ty = TypeCheck.TypedT (Just ty) UniverseF

pattern UnitTypeT :: TypedTerm b a -> TypedTerm b a
pattern UnitTypeT ty = TypeCheck.TypedT (Just ty) UnitTypeF

pattern UnitT :: TypedTerm b a -> TypedTerm b a
pattern UnitT ty = TypeCheck.TypedT (Just ty) UnitF

-- | A dependent product type (\(\Pi\)-type): \(\prod_{x : A} B(x)).
pattern FunT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern FunT ty a b = TypeCheck.TypedT (Just ty) (FunF a b)

-- | A \(\lambda\)-abstraction.
pattern LamT :: TypedTerm b a -> Maybe (TypedTerm b a) -> ScopedTypedTerm b a -> TypedTerm b a
pattern LamT ty argType body = TypeCheck.TypedT (Just ty) (LamF argType body)

-- | An application of one term to another.
pattern AppT :: TypedTerm b a -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
pattern AppT ty t1 t2 = TypeCheck.TypedT (Just ty) (AppF t1 t2)

universeT :: TypedTerm b a
universeT = TypeCheck.TypedT Nothing UniverseF

-- | Abstract over one variable in a term.
--
-- >>> lam "x" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → f x₁
-- >>> lam "f" (App (Var "f") (Var "x")) :: Term String String
-- λx₁ → x₁ x
lam :: Eq a => Maybe (Term a a) -> a -> Term a a -> Term a a
lam ty x body = Lam ty (abstract1Name x body)

lam_ :: Eq a => a -> Term a a -> Term a a
lam_ x body = Lam Nothing (abstract1Name x body)

let_ :: Eq a => Term a a -> a -> Term a a -> Term a a
let_ u x body = Let u (abstract1Name x body)

type TypeInfo' = TypeInfo Rzk.Var UTypedTerm' Rzk.Var
type TypeInfoInScope'
  = TypeInfo Rzk.Var UTypedTermInScope' (Bound.Var (Name Rzk.Var ()) Rzk.Var)

execTypeCheck' :: TypeCheck' a -> Either TypeError a
execTypeCheck' = TypeCheck.execTypeCheck defaultFreshMetaVars

runTypeCheckOnce' :: TypeCheck' a -> Either TypeError (a, TypeInfo')
runTypeCheckOnce' = TypeCheck.runTypeCheckOnce defaultFreshMetaVars

type TypeCheck' = TypeCheck UTypedTerm' Rzk.Var Rzk.Var
type TypeCheckInScope'
  = TypeCheck UTypedTermInScope' (Bound.Var (Name Rzk.Var ()) Rzk.Var) Rzk.Var

whnfT :: UTypedTerm b a v -> UTypedTerm b a v
whnfT = id

instance TypeCheck.TypeCheckable TermF where
  inferTypeFor = inferTypeForTermF
  whnfT = id
  universeT = TypeCheck.TypedT Nothing UniverseF

inferTypeForTermF
  :: (Eq a, Eq v)
  => TermF
        (TypeCheck (UTypedTermInScope b a v) (Bound.Var (Name b ()) a) v
            (UScopedTypedTerm b a v))
        (TypeCheck (UTypedTerm b a v) a v (UTypedTerm b a v))
  -> TypeCheck (UTypedTerm b a v) a v
        (TypedTermF (UScopedTypedTerm b a v) (UTypedTerm b a v))
inferTypeForTermF term = case term of
  UniverseF -> pure (TypeCheck.TypedF UniverseF (Just universeT))
  -- a -> b
  FunF inferA inferB -> do
    a <- inferA
    _ <- trace "shouldHaveType #2" $ a `shouldHaveType` universeT
    b <- inferB
    _ <- trace "shouldHaveType #3" $ b `shouldHaveType` universeT
    pure (TypeCheck.TypedF (FunF a b) (Just universeT))

  LamF minferTypeOfArg inferBody -> trace "[inferTypeForF LamF]" $ do
    typeOfArg <- case minferTypeOfArg of
      Just inferTypeOfArg -> inferTypeOfArg
      Nothing             -> VarT . UMetaVar <$> freshTypeMetaVar
    typeOfArg' <- trace "shouldHaveType #4" $ typeOfArg `shouldHaveType` universeT
    scopedTypedBody <- typecheckInScope $ do
      assignType (Bound.B (Name Nothing ())) (fmap Bound.F typeOfArg') -- FIXME: unnamed?
      inferBody
    typeOfBody <- typeOfScopedWith typeOfArg' scopedTypedBody >>= nonDep
    typeOfBody' <- trace "shouldHaveType #5" $ typeOfBody `shouldHaveType` universeT
    pure $ TypeCheck.TypedF
      (LamF (typeOfArg <$ minferTypeOfArg) scopedTypedBody)
      (Just (FunT universeT typeOfArg' typeOfBody'))

  AppF infer_f infer_x -> trace "[inferTypeForF AppF]" $ do
    f <- infer_f
    x <- infer_x
    TypeCheck.TypedF (AppF f x) . Just <$> do
      typeOf f >>= \case
        FunT _ argType bodyType -> do
          info <- getTypeInfo
          typeOf_x <- typeOf x
          _ <-
            trace (show (unsafeCoerce x :: UTypedTerm'2) <> " `shouldHaveType` " <> show (unsafeCoerce argType :: UTypedTerm'2)) $
              trace (show (unsafeCoerce info :: TypeInfo'2)) $
                trace (show (unsafeCoerce typeOf_x :: UTypedTerm'2)) $
                  trace (show (unsafeCoerce typeOf_x :: UTypedTerm'2) <> " `unifyWithExpected` " <> show (unsafeCoerce argType :: UTypedTerm'2))
                  x `shouldHaveType` argType
          trace "shouldHaveType #7" $ bodyType `shouldHaveType` universeT
        t@(VarT _) -> do
          bodyType <- VarT . UMetaVar <$> freshTypeMetaVar
          typeOf_x <- typeOf x
          _ <- trace "unifyWithExpected #1" $ t `unifyWithExpected` FunT universeT typeOf_x bodyType
          clarifyTypedTerm bodyType
        _ -> fail "inferTypeForF: application of a non-function"

  UnitTypeF -> pure (TypeCheck.TypedF UnitTypeF (Just universeT))
  UnitF -> pure (TypeCheck.TypedF UnitF (Just (UnitTypeT universeT)))
  LetF inferArg inferBody -> do
    arg <- inferArg
    typeOfArg <- typeOf arg
    typeOfArg' <- typeOfArg `shouldHaveType` universeT
    scopedTypedBody <- typecheckInScope $ do
      assignType (Bound.B (Name Nothing ())) (fmap Bound.F typeOfArg')
      inferBody
    typeOfBody <- typeOfScopedWith typeOfArg' scopedTypedBody >>= nonDep
    typeOfBody' <- typeOfBody `shouldHaveType` universeT
    pure $ TypeCheck.TypedF
      (LetF arg scopedTypedBody)
      (Just typeOfBody')

infer' :: Term' -> TypeCheck' UTypedTerm'
infer' = TypeCheck.infer

inferScoped' :: ScopedTerm' -> TypeCheck' UScopedTypedTerm'
inferScoped' = TypeCheck.inferScoped

inferInScope' :: TermInScope' -> TypeCheck' UTypedTermInScope'
inferInScope' = fmap (fmap TypeCheck.dist') . typecheckInScope . typecheckDist . TypeCheck.infer

instance Unifiable TermF where
  zipMatch (AppF f1 x1) (AppF f2 x2)
    = Just (AppF (Right (f1, f2)) (Right (x1, x2)))

  zipMatch (LamF argTy1 body1) (LamF argTy2 body2)
    = Just (LamF argTy (Right (body1, body2)))
    where
      argTy =
        case (argTy1, argTy2) of
          (Nothing, _)     -> Left <$> argTy2
          (_, Nothing)     -> Left <$> argTy1
          (Just x, Just y) -> Just (Right (x, y))

  zipMatch (FunF arg1 body1) (FunF arg2 body2)
    = Just (FunF (Right (arg1, arg2)) (Right (body1, body2)))

  zipMatch UniverseF UniverseF = Just UniverseF

  zipMatch UnitTypeF UnitTypeF = Just UnitTypeF
  zipMatch UnitF UnitF = Just UnitF

  zipMatch (LetF u1 t1) (LetF u2 t2)
    = Just (LetF (Right (u1, u2)) (Right (t1, t2)))

  zipMatch FunF{} _ = Nothing
  zipMatch LamF{} _ = Nothing
  zipMatch UniverseF{} _ = Nothing
  zipMatch AppF{} _ = Nothing
  zipMatch UnitTypeF{} _ = Nothing
  zipMatch UnitF{} _ = Nothing
  zipMatch LetF{} _ = Nothing

  appSome _ []     = error "cannot apply to zero arguments"
  appSome f (x:xs) = (AppF f x, xs)

  unAppSome (AppF f x) = Just (f, [x])
  unAppSome _          = Nothing

  abstract = LamF (error "argument type")

instance Pretty Rzk.Var where
  pretty (Rzk.Var x) = pretty x

instance (Pretty n, Pretty b) => Pretty (Name n b) where
  pretty (Name Nothing b)     = pretty b
  pretty (Name (Just name) b) = "<" <> pretty name <> " " <> pretty b <> ">"

instance (Pretty b, Pretty a) => Pretty (Bound.Var b a) where
  pretty (Bound.B b) = "<bound " <> pretty b <> ">"
  pretty (Bound.F x) = "<free " <> pretty x <> ">"

instance IsString a => IsString (Bound.Var b a) where
  fromString = Bound.F . fromString

-- | Uses 'Pretty' instance.
--
-- >>> mkLams 5 (abstract (const Nothing) (Var "y")) :: Term String String
-- λx₁ → λx₂ → λx₃ → λx₄ → λx₅ → y
instance (Pretty a, Pretty b, IsString a) => Show (Term b a) where
  show = show . pretty

-- | Uses default names (@x@ with a positive integer subscript) for bound variables:
--
-- >>> pretty (mkLams 5 (abstract (const Nothing) (Var "y")) :: Term String String)
-- λx₁ → λx₂ → λx₃ → λx₄ → λx₅ → y
instance (Pretty a, Pretty b, IsString a) => Pretty (Term b a) where
  pretty = ppTerm defaultFreshVars

defaultFreshVars :: IsString a => [a]
defaultFreshVars = mkDefaultFreshVars "x"

defaultFreshMetaVars :: IsString a => [a]
defaultFreshMetaVars = mkDefaultFreshVars "M"

mkDefaultFreshVars :: IsString a => String -> [a]
mkDefaultFreshVars prefix = [ fromString (prefix <> toIndex i) | i <- [1..] ]
  where
    toIndex n = index
      where
        digitToSub c = chr ((ord c - ord '0') + ord '₀')
        index = map digitToSub (show n)

instance (Pretty a, Pretty b, IsString a) => Show (TypedTerm b a) where
  show = \case
    FreeScoped (TypeCheck.TypedF term ty) -> show (FreeScoped (bimap untypedScoped untyped term)) <> " : " <> show (untyped (fromMaybe universeT ty))
    t -> show (untyped t)

ppTypedTerm :: (Pretty a, Pretty b) => [a] -> TypedTerm b a -> Doc ann
ppTypedTerm vars = ppTerm vars . untyped

-- | Pretty-print an untyped term.
ppTerm :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTerm vars = \case
  Var x -> pretty x

  Universe -> "U"

  Fun a b -> ppTermFun vars a <+> "→" <+> ppTerm vars b
  Lam Nothing body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  Lam (Just ty) body -> ppScopedTerm vars body $ \x body' ->
    "λ" <> parens (pretty x <+> ":" <+> ppTerm vars ty) <+> "→" <+> body'
  App f x -> ppTermFun vars f <+> ppTermArg vars x

  UnitType -> "UNIT"
  Unit -> "unit"
  Let u t -> ppScopedTerm vars t $ \x t' ->
    align (hsep ["let" <+> pretty x <+> "=" <+> ppTerm vars u <+> "in", t'])

ppElimWithArgs :: (Pretty a, Pretty b) => [a] -> Doc ann -> [Term b a] -> Doc ann
ppElimWithArgs vars name args = name <> tupled (map (ppTermFun vars) args)

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Var{} -> ppTerm vars t
  t@App{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t

  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@Fun{} -> Doc.parens (ppTerm vars t)
  t@Let{} -> Doc.parens (ppTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppTermArg :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermArg vars = \case
  t@Var{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t

  t@App{} -> Doc.parens (ppTerm vars t)
  t@Lam{} -> Doc.parens (ppTerm vars t)
  t@Fun{} -> Doc.parens (ppTerm vars t)
  t@Let{} -> Doc.parens (ppTerm vars t)

ppScopedTerm
  :: (Pretty a, Pretty b)
  => [a] -> ScopedTerm b a -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScopedTerm [] _ _            = error "not enough fresh names"
ppScopedTerm (x:xs) t withScope = withScope x (ppTerm xs (Scope.instantiate1 (Var x) t))

examples :: IO ()
examples = mapM_ runExample . zip [1..] $
  [ let_ (lam_ "f" $ lam_ "z" $ Var "z") "zero" $
    let_ (lam_ "n" $ lam_ "f" $ lam_ "z" $ App (Var "f") (App (App (Var "n") (Var "f")) (Var "z"))) "succ" $
      App (Var "succ") (App (Var "succ") (Var "zero"))

  , let_ (lam_ "f" $ lam_ "z" $ Var "z") "zero" $
      Var "zero"

  , App (lam_ "x" (Var "x")) $
      lam_ "f" $ lam_ "z" $ Var "z"

  , let_ Unit "x" Unit

  , let_ Unit "x" (Var "x")


  , lam Nothing "f" $
      lam Nothing "x" $
        App (Var "f") (Var "x") -- ok (fixed)

  , lam (Just (Fun UnitType UnitType)) "f" $
      lam (Just UnitType) "x" $
        App (Var "f") (Var "x") -- ok

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      lam (Just (Var "A")) "x" $
        App (Var "f") (Var "x") -- ok (fixed)

  , lam Nothing "x" $
      lam Nothing "x" $
        Var "x" -- ok

  , lam Nothing "x" $
      lam Nothing "y" $
        Var "x" -- ok (fixed)

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      lam Nothing "x" $
        App (Var "f") (Var "x") -- ok

  , lam Nothing "x" $
      Var "x" -- ok

  , lam (Just (Var "A")) "x" $
      Var "x" -- ok

  , lam (Just (Fun (Var "A") (Var "B"))) "f" $
      Var "f" -- ok

  , lam Nothing "f" $
      App (Var "f") (Var "f")  -- ok: type error

  , lam Nothing "f" $
      App (Var "f") Unit -- ok

  , Fun (Var "A") UnitType -- ok (looped because of unsafeCoerce)
  , Fun (Var "A") (Var "B") -- ok (looped because of unsafeCoerce)

  , lam Nothing "f" $
      lam (Just UnitType) "x" $
        App (Var "f") (App (Var "f") (Var "x"))
        -- ok

  , Unit                  -- ok
  , App Unit Unit         -- type error
  , UnitType              -- ok
  , Var "x"               -- ok-ish
  , App (Var "f") Unit    -- ambiguous

  , App (Var "f") (App (Var "f") Unit) -- ok (fixed)

  , Fun Unit Unit         -- type error
  , Fun UnitType UnitType -- ok

  , Var "x"
  , App (Var "f") (Var "x")
  , lam (Just Unit) "x" (Var "x")
  , lam (Just Unit) "x" (Var "y")
  , lam (Just (Var "A")) "x" (Var "x")
  , lam (Just (Fun (Var "A") (Var "B"))) "x" (Var "x")
  ]
  where
    runExample :: (Int, Term') -> IO ()
    runExample (n, term) = do
      putStrLn ("Example #" <> show n <> ":")
      -- putStr   "[input term]:          "
      print term
      -- _ <- getLine
      -- putStr   "[with inferred types]: "
      case runTypeCheckOnce' (TypeCheck.infer term) of
        Left err -> putStrLn ("Type Error: " <> show err)
        Right (typedTerm, typeInfo) -> do
          print typedTerm
          print typeInfo
      putStrLn ""
      _ <- getLine
      return ()

deriveBifunctor ''TermF
deriveBifoldable ''TermF
deriveBitraversable ''TermF
