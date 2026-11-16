{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic folds over 'Data.Data' syntax trees.
module Language.Rzk.Data
  ( foldData
  ) where

import           Data.Data (Data, gmapQ)

-- | Fold a value and all of its children in pre-order. The query may use
-- 'Data.Data.cast' to select a particular node type.
foldData :: forall a m. (Data a, Monoid m) => (forall b. Data b => b -> m) -> a -> m
foldData query = go
  where
    go :: Data b => b -> m
    go x = query x <> mconcat (gmapQ go x)
