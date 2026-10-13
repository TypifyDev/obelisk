{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

-- | Cookie access as an Effectful effect.
--
-- Honest port of the previous MTL 'HasCookies' class. The MTL surface
-- (class + 'CookiesT' newtype transformer + instances for every
-- MTL transformer) is deleted; the transformers it wrapped
-- (BehaviorWriterT, DynamicWriterT, EventWriterT, PostBuildT, QueryT,
-- ReaderT, RequesterT, RouteToUrlT, SetRouteT, StaticDomBuilderT,
-- TriggerEventT, RoutedT, ConfigsT, HydrationDomBuilderT) do not exist
-- in the effectful port.
--
-- Consumers previously written against @HasCookies m@ should switch to
-- the @CookiesEff :> es@ constraint from
-- 'Jenga.Frontend.Effectful.Cookies', re-exported here.
module Jenga.Frontend.Cookie
  ( CookiesEff (..)
  , Cookies
  , askCookies
  , runCookies
  , readCookiesFromDocument
  -- * Upstream MTL surface — Eff-shape ports
  , HasCookies
  , CookiesT
  , runCookiesT
  , mapCookies
  ) where

import           Data.Kind                              (Constraint)
import           Data.Text.Encoding                     (encodeUtf8)
import qualified Effectful                              as Eff
import           Effectful                              (Eff, (:>))
import qualified Effectful.Dispatch.Dynamic             as ED
import           "ghcjs-dom" GHCJS.DOM.Document          (getCookie)
import qualified GHCJS.DOM                              as DOM
import           Web.Cookie                             (parseCookies)

import           Reflex.Effectful.DomHost                (JSM', liftJSM)

import           Jenga.Frontend.Effectful.Cookies

-- | Read cookies from the current document via JSaddle. Intended to be
-- called once at boot; the result is typically fed into 'runCookies' to
-- install the 'CookiesEff' interpreter for the widget tree.
readCookiesFromDocument :: JSM' :> es => Eff.Eff es Cookies
readCookiesFromDocument = liftJSM $ do
  mdoc <- DOM.currentDocument
  case mdoc of
    Nothing -> pure []
    Just doc -> do
      raw <- getCookie doc
      pure $ parseCookies (encodeUtf8 raw)

-- ==================================================================
-- Upstream MTL surface — Eff-shape ports (source-compat aliases +
-- one new function via 'interpose').
-- ==================================================================

-- | Constraint-alias form of upstream 'HasCookies m'. Upstream's class
-- had one method — 'askCookies' — so the direct Effectful analog is
-- simply @'CookiesEff' :> es@ (the effect being in the row).
type HasCookies (es :: [Eff.Effect]) = (CookiesEff :> es :: Constraint)

-- | Source-compat alias for consumers written against upstream's
-- 'CookiesT' monad transformer. Effectful has no per-effect transformer
-- — the effect *row* is the equivalent — but pinning the name to the
-- effect constructor lets grep and manual porting find the analog
-- without hunting.
type CookiesT = CookiesEff

-- | Source-compat alias for 'runCookies'. Upstream's 'runCookiesT'
-- takes 'Cookies' and discharges the transformer; Effectful's
-- 'runCookies' takes 'Cookies' and interprets the effect. Same shape.
runCookiesT :: Cookies -> Eff (CookiesEff : es) a -> Eff es a
runCookiesT = runCookies

-- | Effectful port of upstream 'mapCookiesT'. Transform the ambient
-- 'Cookies' value seen by every 'askCookies' call inside the wrapped
-- action, without re-interpreting the whole effect. Implemented via
-- 'ED.interpose' — the outer interpreter still governs anything not
-- overridden here.
--
-- Not exactly upstream's shape (upstream's operates on the inner monad
-- via 'mapReaderT'); this is the direct Effectful equivalent of what
-- callers typically want: a lens-style transformer over the ambient
-- state.
mapCookies
  :: CookiesEff :> es
  => (Cookies -> Cookies)
  -> Eff es a
  -> Eff es a
mapCookies f = ED.interpose $ \_ -> \case
  AskCookies -> f <$> askCookies
