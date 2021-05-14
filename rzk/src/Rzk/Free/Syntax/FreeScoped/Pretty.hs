{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rzk.Free.Syntax.FreeScoped.Pretty where

import           Bound.Scope
import           Bound.Var
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Text.Prettyprint.Doc
import           Rzk.Free.Syntax.FreeScoped

-- | Pretty-print an untyped term.
prettyFreeScoped
  :: forall a b term ann.
    (Pretty a, Pretty (term ([(b, a)], Doc ann) (Doc ann)), Bifunctor term, Bifoldable term)
  => [b -> a] -> FreeScoped b term a -> Doc ann
prettyFreeScoped vars = \case
  PureScoped x -> pretty x
  FreeScoped f ->
    let f' = bimap (prettyScoped vars) (prettyFreeScoped vars) f
           :: term ([(b, a)], Doc ann) (Doc ann)
     in pretty f'

prettyScoped
  :: (Pretty a, Pretty (term ([(b, a)], Doc ann) (Doc ann)), Bifunctor term, Bifoldable term)
  => [b -> a] -> Scope b (FreeScoped b term) a -> ([(b, a)], Doc ann)
prettyScoped [] _     = error "prettyScoped: not enough fresh names"
prettyScoped (x:xs) t =
  (zip bvars (map x bvars), prettyFreeScoped xs (instantiate (PureScoped . x) t))
  where
    bvars = foldMap k (fromScope t)

    k (B b) = [b]
    k _     = []
