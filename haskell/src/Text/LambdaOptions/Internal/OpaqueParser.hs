{-# LANGUAGE FlexibleInstances #-}

module Text.LambdaOptions.Internal.OpaqueParser (
    OpaqueParser,
    GetOpaqueParsers(..),
) where


import Data.Proxy
import Data.Typeable hiding (typeRep)
import Text.LambdaOptions.Internal.Opaque
import Text.LambdaOptions.Parseable


--------------------------------------------------------------------------------


decomposeFuncProxy :: Proxy (a -> b) -> (Proxy a, Proxy b)
decomposeFuncProxy _ = (Proxy, Proxy)


--------------------------------------------------------------------------------


type OpaqueParser = [String] -> (Maybe Opaque, Int)


parseOpaque :: (Parseable a, Typeable a) => Proxy a -> OpaqueParser
parseOpaque proxy str = case parse str of
    (Nothing, n) -> (Nothing, n)
    (Just x, n) -> (Just $ Opaque $ x `asProxyTypeOf` proxy, n)


--------------------------------------------------------------------------------


class GetOpaqueParsers f where
    getOpaqueParsers :: Proxy f -> [(TypeRep, OpaqueParser)]


instance (Parseable a, Typeable a, GetOpaqueParsers b) => GetOpaqueParsers (a -> b) where
    getOpaqueParsers funcProxy = let
        (proxyA, proxyB) = decomposeFuncProxy funcProxy
        typeRep = typeOf proxyA
        parser = parseOpaque proxyA
        in (typeRep, parser) : getOpaqueParsers proxyB


instance (Monad m) => GetOpaqueParsers (m a) where
    getOpaqueParsers ~Proxy = []




