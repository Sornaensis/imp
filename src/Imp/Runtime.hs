{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Runtime support for the imperative DSL
-- Provides ref abstractions, lens composition, and capability helpers
module Imp.Runtime
  ( -- * Ref abstraction
    Ref(..)
  , (.^)
  , getRef
  , setRef
  , modRef
  , Lens'
  , lens
  , set
  , view
  , ix
  , impAdd
  , ImpAdd(..)
  , impIndex
  , ImpIndex(..)
  
    -- * Capability helpers
  , requireCap
  , taggedLiteral
  ) where

import Control.Lens (Lens', lens, set, view, ix, use, (.=), (%=))
import Control.Monad.State.Class (MonadState)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- * Ref abstraction

-- | A reference to a field in the state, represented as a lens
newtype Ref s a = Ref { unRef :: Lens' s a }

-- | Compose lens paths
(.^) :: Ref s a -> Lens' a b -> Ref s b
Ref l .^ l2 = Ref (l . l2)

infixl 8 .^

-- | Get the value at a reference
getRef :: MonadState s m => Ref s a -> m a
getRef (Ref l) = use l

-- | Set the value at a reference
setRef :: MonadState s m => Ref s a -> a -> m ()
setRef (Ref l) a = l .= a

-- | Modify the value at a reference
modRef :: MonadState s m => Ref s a -> (a -> a) -> m ()
modRef (Ref l) f = l %= f

-- * Capability helpers

-- | Generic capability requirement helper
-- Used to force constraints into inferred types
requireCap :: Monad m => m ()
requireCap = pure ()

-- * Tagged literal hook

-- | Default tagged literal hook (identity)
-- Users can redefine this in scope to customize tagged literal semantics.
taggedLiteral :: Text -> a -> a
taggedLiteral _ = id

-- * Operator defaults

-- | Default addition operator, supporting numeric addition and text concatenation.
impAdd :: ImpAdd a => a -> a -> a
impAdd = impAdd'

class ImpAdd a where
  impAdd' :: a -> a -> a

instance Num a => ImpAdd a where
  impAdd' = (+)

instance ImpAdd Text where
  impAdd' = (<>)

instance ImpAdd String where
  impAdd' = (<>)

-- * Indexing helpers

class ImpIndex c i a | c i -> a where
  impIndex :: c -> i -> Maybe a

instance Integral i => ImpIndex [a] i a where
  impIndex xs i =
    let n = fromIntegral i :: Int
    in if n < 0
         then Nothing
         else listToMaybe (drop n xs)

instance Ord k => ImpIndex (Map k v) k v where
  impIndex = flip Map.lookup
