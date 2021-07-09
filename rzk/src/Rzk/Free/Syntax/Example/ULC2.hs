{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Free.Syntax.Example.ULC2 where

import           Data.Bifunctor.TH

import           Rzk.Free.Syntax.FreeScoped.Unification2
-- import           Rzk.Weakly

-- data WithSharedWHNF term scope subterm = WithSharedWHNF
--   { original :: term scope subterm
--   , whnf     :: Weakly subterm
--   } deriving (Functor, Foldable, Traversable)
--
-- instance Unifiable t => Unifiable (WithSharedWHNF t) where
--   zipMatch w1 w2 = do
--     o <- zipMatch (original w1) (original w2)
--     return (WithSharedWHNF o)
--
-- deriveBifunctor ''WithSharedWHNF
-- deriveBifoldable ''WithSharedWHNF
-- deriveBitraversable ''WithSharedWHNF
