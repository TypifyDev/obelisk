-- | Utilities for generating JS function call strings and
-- implementing static-dynamic DOM (pre-rendering multiple DOM states
-- as HTML strings, then generating JS calls to swap between them).
--
-- The @js2@-@js9@ family builds JS call expressions using Haskell's
-- 'Show' instances and the @IStr@ quasi-quoter for interpolation.
-- Arguments are formatted via 'Show', so 'String' values get
-- Haskell-style double-quoted escaping.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Lamarckian.JS where

import Lamarckian.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T


import Text.IStr
import Data.Some (Some(..))

-- A simple indexed GADT (example code demonstrating Data.Some usage)
data Value' a where
  VInt    :: Int    -> Value' Int
  VBool   :: Bool   -> Value' Bool
  VString :: String -> Value' String

deriving instance Show (Value' a)

-- Wrap examples into Some
store :: [Some Value']
store =
  [ Some (VInt 1)
  , Some (VBool False)
  , Some (VString "hi")
  ]

-- Apply one "step" to each case and return a *String* as result
step :: Some Value' -> String
step (Some v) = case v of
  VInt n    -> "incremented: " ++ show (n + 1)
  VBool b   -> "negated: " ++ show (not b)
  VString s -> "shouted: " ++ map toUpper s
  where
    toUpper c
      | 'a' <= c && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- main :: IO ()
-- main = mapM_ (putStrLn . step) store

-- | Opaque JS source string. 'Semigroup'/'Monoid' via string concatenation.
newtype Script = Script String deriving (Semigroup, Monoid)



data JSObject = JSObject String
-- | Collect a JS reference and do something with it
-- | Enforces referential transparency is the purpose
data SyntaxJS a where
  With :: SyntaxJS (JSObject, (JSObject -> a))
  -- WithAPI :: ?


-- | A single JS function call expression string, e.g. @\"setChild(\\\"myDiv\\\",\\\"<div>...</div>\\\")\"@.
newtype JSFunc = JSFunc String deriving Show


-- | Build a JS function call with 2 arguments: @funcName(a, b)@.
-- Uses 'Show' to format each argument.
js2 :: (Show a, Show b) => Name -> a -> b -> JSFunc
js2 func a b = JSFunc [istr| #{func}#{(a, b)} |]

js3 :: (Show a, Show b, Show c) => Name -> a -> b -> c -> JSFunc
js3 func a b c = JSFunc [istr| #{func}#{(a, b, c)} |]

js4 :: (Show a, Show b, Show c, Show d) => Name -> a -> b -> c -> d -> JSFunc
js4 func a b c d = JSFunc [istr| #{func}#{(a, b, c, d)} |]
  
js5 :: (Show a, Show b, Show c, Show d, Show e) => Name -> a -> b -> c -> d -> e -> JSFunc
js5 func a b c d e = JSFunc [istr| #{func}#{(a, b, c, d, e)} |]

js6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Name -> a -> b -> c -> d -> e -> f -> JSFunc
js6 func a b c d e f = JSFunc [istr| #{func}#{(a, b, c, d, e, f)} |]

js7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Name -> a -> b -> c -> d -> e -> f -> g -> JSFunc
js7 func a b c d e f g = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g)} |]

js8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Name -> a -> b -> c -> d -> e -> f -> g -> h -> JSFunc
js8 func a b c d e f g h = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g, h)} |]

js9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Name -> a -> b -> c -> d -> e -> f -> g -> h -> i -> JSFunc
js9 func a b c d e f g h i = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g, h, i)} |]

type DomID = String

-- | Wrap a list of already-rendered HTML fragments as
-- @setChild(domId, \"<html>...\")@ JS calls.
--
-- Callers pre-render each 'StaticWidgetE' with 'runStaticWidget' before
-- passing the resulting ByteStrings in — a list of rank-2 widgets
-- cannot be expressed directly as @[StaticWidgetE ()]@ because the
-- element type is polymorphic (GHC forbids impredicative list
-- elements).
staticDynamicDOM :: DomID -> [BS.ByteString] -> [JSFunc]
staticDynamicDOM domId rendered =
  [ js2 "setChild" domId (T.decodeUtf8 x) | x <- rendered ]

