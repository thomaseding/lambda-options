{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Text.LambdaOptions.Internal.OpaqueParser (
  OpaqueParser,
  GetOpaqueParsers,
  getOpaqueParsers,
) where

import           Data.Proxy
                  ( Proxy(Proxy) )
import           Data.Typeable
                  ( Typeable, TypeRep, typeRep )
import           Text.LambdaOptions.Internal.Opaque
                  ( Opaque(Opaque) )
import           Text.LambdaOptions.Parseable
                  ( Parseable(parse) )
import           Type.Funspection
                  ( TaggedReturn, Return )

--------------------------------------------------------------------------------

type OpaqueParser = [String] -> (Maybe Opaque, Int)

parseOpaque :: forall a. (Parseable a, Typeable a) => Proxy a -> OpaqueParser
parseOpaque ~Proxy str = case parse str of
  (Nothing, n) -> (Nothing, n)
  (Just (x::a), n) -> (Just $ Opaque x, n)

--------------------------------------------------------------------------------

class GetOpaqueParsers' r f where
  getOpaqueParsers' :: Proxy r -> Proxy f -> [(TypeRep, OpaqueParser)]

instance (Parseable a, Typeable a, GetOpaqueParsers' r b) => GetOpaqueParsers' r (a -> b) where
  getOpaqueParsers' proxyR ~Proxy = let
    proxyA = Proxy :: Proxy a
    proxyB = Proxy :: Proxy b
    rep = typeRep proxyA
    parser = parseOpaque proxyA
    in (rep, parser) : getOpaqueParsers' proxyR proxyB

instance GetOpaqueParsers' r (Return r) where
  getOpaqueParsers' ~Proxy ~Proxy = []

--------------------------------------------------------------------------------

type GetOpaqueParsers r f = GetOpaqueParsers' r (TaggedReturn r f)

getOpaqueParsers :: forall r f. (GetOpaqueParsers r f) => Proxy r -> Proxy f -> [(TypeRep, OpaqueParser)]
getOpaqueParsers proxyR ~Proxy = getOpaqueParsers' proxyR (Proxy :: Proxy (TaggedReturn r f))

