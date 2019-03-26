{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Text.LambdaOptions.Internal.Wrap (
    Wrap,
    wrap
) where


import Data.Typeable
import Text.LambdaOptions.Internal.Opaque
import Type.Funspection


--------------------------------------------------------------------------------


internalError :: a
internalError = error "InternalError: Text.LambdaOptions.Internal.Wrap"


--------------------------------------------------------------------------------


class Wrap' r f' f where
    wrap' :: Proxy f' -> f -> OpaqueCallback r


instance (Typeable a, Wrap' r b' b) => Wrap' r (a -> b') (a -> b) where
    wrap' ~Proxy f = \case
        Opaque o : os -> case cast o of
            Just x -> let
                p = Proxy :: Proxy b'
                g = wrap' p $ f x
                in g os
            Nothing -> internalError
        [] -> internalError


instance Wrap' r (Return r) r where
    wrap' ~Proxy r = \case
        [] -> r
        _ -> internalError


--------------------------------------------------------------------------------


type Wrap r f = Wrap' r (TaggedReturn r f) f


wrap :: forall r f. (Wrap r f) => f -> OpaqueCallback r
wrap = wrap' (Proxy :: Proxy (TaggedReturn r f)) 


