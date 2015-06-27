{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.LambdaOptions.Internal.Wrap (
    Wrap(..),
) where


import Data.Typeable
import Text.LambdaOptions.Internal.Opaque


--------------------------------------------------------------------------------


internalError :: a
internalError = error "InternalError: Text.LambdaOptions.Internal.Wrap"


--------------------------------------------------------------------------------


class Wrap r f | f -> r where
    wrap :: f -> OpaqueCallback r


instance (Monad m) => Wrap (m b) (m b) where
    wrap action opaques = case opaques of
        [] -> action
        _ -> internalError


instance (Monad m, Typeable a, Wrap (m b) f) => Wrap (m b) (a -> f) where
    wrap f opaques = case opaques of
        Opaque o : os -> case cast o of
            Just x -> let
                g = f x
                g' = wrap g
                in g' os
            Nothing -> internalError
        [] -> internalError






