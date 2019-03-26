{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Text.LambdaOptions.Internal.Return (
    Return(..),
    ReturnOf,
    TaggedReturn,
    TagReturn,
    tagReturn,
) where


import Data.Proxy


--------------------------------------------------------------------------------


newtype Return a = Return a


type family (ReturnOf f) :: * where
    ReturnOf (a -> b) = ReturnOf b
    ReturnOf r = r


type family (TaggedReturn f) :: * where
    TaggedReturn (a -> b) = a -> TaggedReturn b
    TaggedReturn r = Return r


--------------------------------------------------------------------------------


class TagReturn' f' f where
    tagReturn' :: Proxy f' -> f -> f'


instance (TagReturn' b' b) => TagReturn' (a -> b') (a -> b) where
    tagReturn' ~Proxy f = tagReturn' (Proxy :: Proxy b') . f


instance TagReturn' (Return r) r where
    tagReturn' ~Proxy = Return


--------------------------------------------------------------------------------


type TagReturn f = TagReturn' (TaggedReturn f) f


tagReturn :: forall f. (TagReturn f) => f -> TaggedReturn f
tagReturn = tagReturn' (Proxy :: Proxy (TaggedReturn f))


