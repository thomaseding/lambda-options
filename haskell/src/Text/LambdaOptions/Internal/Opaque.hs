{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Text.LambdaOptions.Internal.Opaque (
  Opaque(..),
  OpaqueCallback,
) where

import           Data.Typeable
                  ( Typeable )

--------------------------------------------------------------------------------

data Opaque where
  Opaque :: (Typeable a) => a -> Opaque

type OpaqueCallback r = [Opaque] -> r

