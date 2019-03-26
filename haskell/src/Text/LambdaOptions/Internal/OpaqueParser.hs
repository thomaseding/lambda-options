{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Text.LambdaOptions.Internal.OpaqueParser (
    OpaqueParser,
    GetOpaqueParsers,
    getOpaqueParsers,
) where


import Data.Proxy
import Data.Typeable
import Text.LambdaOptions.Internal.Opaque
import Text.LambdaOptions.Internal.Return
import Text.LambdaOptions.Parseable


--------------------------------------------------------------------------------


type OpaqueParser = [String] -> (Maybe Opaque, Int)


parseOpaque :: forall a. (Parseable a, Typeable a) => Proxy a -> OpaqueParser
parseOpaque ~Proxy str = case parse str of
    (Nothing, n) -> (Nothing, n)
    (Just (x::a), n) -> (Just $ Opaque x, n)


--------------------------------------------------------------------------------


class GetOpaqueParsers' f where
    getOpaqueParsers' :: Proxy f -> [(TypeRep, OpaqueParser)]


instance (Parseable a, Typeable a, GetOpaqueParsers' b) => GetOpaqueParsers' (a -> b) where
    getOpaqueParsers' ~Proxy = let
        proxyA = Proxy :: Proxy a
        proxyB = Proxy :: Proxy b
        rep = typeRep proxyA
        parser = parseOpaque proxyA
        in (rep, parser) : getOpaqueParsers' proxyB


instance GetOpaqueParsers' (Return r) where
    getOpaqueParsers' ~Proxy = []


--------------------------------------------------------------------------------


type GetOpaqueParsers f = GetOpaqueParsers' (TaggedReturn f)


getOpaqueParsers :: forall f. (GetOpaqueParsers f) => Proxy f -> [(TypeRep, OpaqueParser)]
getOpaqueParsers ~Proxy = getOpaqueParsers' (Proxy :: Proxy (TaggedReturn f))


