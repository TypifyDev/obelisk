{-# LANGUAGE PackageImports #-}

-- | Functions for running 'StaticWidget'' values to 'ByteString' HTML.
--
-- The core runner is 'runStaticWidget' which strips 'SetRouteT' and
-- 'RouteToUrlT', calls @renderStatic@, and returns the raw bytes.
-- Higher-level variants apply template substitution ('runStaticTemplateWidget',
-- 'runStaticHTemplateWidget') or prepend a doctype ('createHtmlDoc').
module Lamarckian.Render where

import Lamarckian.Types
import Lamarckian.Template as Template

import Jenga.Route.FrontendMTL (runSetRouteT, runRouteToUrlT)
import Reflex.Dom.Core
import "template-haskell" Language.Haskell.TH
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

-- | Render a 'StaticWidget'' to 'ByteString' and prepend @\<!DOCTYPE html\>@.
createHtmlDoc :: StaticWidget' t r () -> IO BS.ByteString
createHtmlDoc dom = do
  html <- runStaticWidget dom
  pure $ "<!DOCTYPE html>" <> html

-- | Like 'unTemplate' but takes a map of 'HtmlString' values,
-- unwrapping them to plain 'String' before substitution.
unTemplateHTMLString :: Map.Map String HtmlString -> String -> String
unTemplateHTMLString tmplMap input = Template.unTemplate (T.unpack . getHtml <$> tmplMap) input

-- | Render a widget at compile time (Template Haskell splice) with
-- template substitution applied. The resulting 'ByteString' is baked into the binary.
--
-- TODO use ReaderT to ensure use of templateSlot matches existing key
renderStaticTemplate' :: TemplateVars -> StaticWidget' r x () -> Q BS.ByteString
renderStaticTemplate' mappy dom = do
  container <- runIO $ runStaticWidget dom
  pure
    $ T.encodeUtf8 . T.pack 
    $ unTemplate mappy
    $ T.unpack . T.decodeUtf8 
    $ container

-- | Run a 'StaticWidget'' to 'ByteString' by stripping 'SetRouteT' and
-- 'RouteToUrlT' layers, then calling @renderStatic@.
--
-- Note: @renderRoute@ is @const \"\"@, so any @routeToUrl@ calls inside the
-- widget will produce empty strings.
runStaticWidget :: StaticWidget' r x a -> IO BS.ByteString
runStaticWidget = fmap snd . renderStatic . flip runRouteToUrlT renderRoute . runSetRouteT
  where
    renderRoute = const "" -- doesnt seem to do anything anyways

-- | Like 'runStaticWidget' but applies 'unTemplate' substitution afterwards.
runStaticTemplateWidget :: TemplateVars -> StaticWidget' r x () -> IO BS.ByteString
runStaticTemplateWidget mappy dom = do
  container <- runStaticWidget dom
  pure
    $ T.encodeUtf8 . T.pack 
    $ Template.unTemplate mappy
    $ T.unpack . T.decodeUtf8 
    $ container

-- | Two-phase rendering for heterogeneous templates:
-- strips 'HTemplateVars' down to 'HTemplateRefs' (keys + annotations only),
-- passes those refs to the DOM builder, renders, then substitutes all slot
-- values by their hash-derived keys.
--
-- Note: this function unconditionally prints debug output to stdout
-- via @putStrLn@ and @print@.
runStaticHTemplateWidget
  :: HTemplateVars k a
  -> (HTemplateRefs k a -> StaticWidget' r x ())
  -> IO BS.ByteString
runStaticHTemplateWidget mappy mkDom = do
  container <- runStaticWidget $ mkDom (justReferences mappy) --dom
  x <- pure
    $ T.encodeUtf8 . T.pack 
    $ unTemplateHTMLString (getHTemplateValues mappy)
    $ T.unpack . T.decodeUtf8 
    $ container
  putStrLn "runEmailHTemplateWidget"
  print x
  pure x
  where
    justReferences :: HTemplateVars k a -> HTemplateRefs k a
    justReferences = fmap (fmap grab_1_3)

    grab_1_3 (x,_,a) = (x,a)

