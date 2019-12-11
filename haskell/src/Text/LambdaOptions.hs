{-# LANGUAGE Safe #-}

-- | Declarative command-line parser using type-driven pattern matching.
module Text.LambdaOptions (
  module Text.LambdaOptions.Core,
  module Text.LambdaOptions.Formatter,
  module Text.LambdaOptions.Keyword,
  module Text.LambdaOptions.Parseable,
  module Text.LambdaOptions.Parseable.Booly,
  module Text.LambdaOptions.Parseable.List,
) where

import Text.LambdaOptions.Core
import Text.LambdaOptions.Formatter
import Text.LambdaOptions.Keyword
import Text.LambdaOptions.Parseable
import Text.LambdaOptions.Parseable.Booly
import Text.LambdaOptions.Parseable.List

