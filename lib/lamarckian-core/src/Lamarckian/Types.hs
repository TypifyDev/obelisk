{-# LANGUAGE PackageImports #-}

-- | Core types for the Lamarckian static site library.
--
-- This module defines the foundational types used across lamarckian-core:
-- route-aware static widgets, template slot substitution maps, static site
-- configuration, and simple result types.
--
-- The central type is 'StaticWidget'', a monad stack that supports
-- 'DomBuilder', route encoding, and serializes to 'ByteString' HTML
-- via @renderStatic@. It runs under GHC only (not GHCJS).
module Lamarckian.Types where

import Jenga.Route.Frontend
import Jenga.Route.FrontendMTL (SetRouteT, RouteToUrlT)
import Reflex.Dom.Core
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import qualified Data.Text as T
import "template-haskell" Language.Haskell.TH

-- | Key for grouping template values (e.g. section identifiers).
type GroupKey = String

-- | Concrete monad stack for running static DOM rendering.
-- Uses 'StaticDomBuilderT' which serializes to 'ByteString', not a live DOM.
type StaticDom = PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))

type StaticPackagePath = FilePath

-- | In theory we could make the r of ReaderT generic
newtype TemplateT m a = TemplateT { runTemplateT :: ReaderT TemplateVars m a }
-- | In generic impl. that would mean `templateSlot :: (r -> a) -> TemplateT a ~~~ asks`
-- | Further, we could also have a templateFileSlot function
type Name = String

-- | A route-aware static widget. This is the central rendering type.
--
-- Satisfies: 'DomBuilder', 'MonadHold', 'PostBuild', 'Prerender' (static side),
-- 'MonadIO', 'SetRoute', 'RouteToUrl'.
--
-- Note: 'routeToUrl' calls inside this widget return @\"\"@ in static rendering
-- because 'runStaticWidget' strips the route layer with @renderRoute = const \"\"@.
-- Pass actual URLs as template vars if you need them in the output HTML.
type StaticWidget' r x a = SetRouteT (SpiderTimeline Global) (R r) (RouteToUrlT (R r) (StaticWidget x)) a

-- | Slot name to replacement string. Used with 'Lamarckian.Template.unTemplate'
-- to fill @{{::=name}}@ placeholders in rendered HTML.
type TemplateVars = Map.Map String String

-- | Content-hash-derived key for a template slot. Typically produced by
-- 'Lamarckian.Template.hashString' which outputs @\"h-\" <> hex(sha256(input))@.
type SlotKey = String

-- | Heterogeneous template vars: keyed groups of @(SlotKey, HtmlString, extra)@.
--
-- The key @k@ allows arbitrary retrieval of a template group.
-- For example, an email with multiple sections requiring non-escaped raw HTML:
--
-- @
-- let tmplMap = fromList [(1, sectionOneVars), (2, sectionTwoVars)]
-- renderSectionOne (Map.findWithDefault [] 1 tmplMap)
-- renderSectionTwo (Map.findWithDefault [] 2 tmplMap)
-- @
type HTemplateVars k a = Map.Map k [(SlotKey, HtmlString, a)]

-- | Like 'HTemplateVars' but with HTML values stripped, leaving only
-- slot keys and annotations. Passed to the DOM builder so it knows
-- which slots exist before values are substituted.
type HTemplateRefs k a = Map.Map k [(SlotKey, a)]

-- | Flat resolved map after rendering: slot keys to their HTML content.
type HTemplateValues = Map.Map SlotKey (HtmlString)

-- | A 'Text' value that has already been rendered as HTML.
-- Not escaped further during template substitution.
newtype HtmlString = HtmlString { getHtml :: T.Text }

-- | Simple three-state async result.
data Promised = NotDone | DoneFailed Reason | DoneSuccess
type Reason = String

data CouldWrite = CanWriteFile FilePath | FailedWriteFile deriving Show

type URL = T.Text

-- | A Template Haskell expression for a file path, resolved at compile time.
type CompiledFilePath = Q Exp

-- | Configuration for a static site deployment.
data StaticSite r = StaticSite
  { _staticSite_baseWritableFolder :: FilePath
  -- ^ the prefix not included in staticFilePath's argument
  , _staticSite_staticFilePath  :: FilePath -> CompiledFilePath
  -- ^ Your Jenga generated staticFilePath function
  , _staticSite_routeEncoder :: R r -> URL
  -- ^ In the simplest of cases, this is just simply rendering the route
  -- ie. (<rendered-route>|"root" if nothing).html
  , _staticSite_subFolder :: Maybe FilePath
  -- ^ Allows for configuring cases like
  -- baseWritableFolder/<subFolder>/filepathFromRoute
  , _staticSite_localHint :: IO Bool
  -- ^ Flag for whether or not to get file directly from
  -- where it was written, which only works if local.
  }

