{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Minimal reproduction of the constraint-solving surface exercised by
-- upstream reflex-dom-core's test/hydration.hs when it hands its
-- @forall m. TestWidget t m => m ()@ body to the port's
-- 'attachHydrationWidgetWithFailure'.
--
-- The point of this file is *iteration speed*: it depends only on the
-- port + reflex-dom-core + reflex, so ghcid can reload it in seconds
-- and surface the missing MTL bridges on 'WidgetEff t x es' one error at a
-- time — without paying the 4-5min transitive-Hackage rebuild the full
-- HydrationBrowser test-suite costs.
--
-- If this file compiles, the WidgetEff MTL surface is complete enough
-- for the upstream 'TestWidget' bundle to typecheck at that monad.
module Main (main) where

import qualified Reflex.Effectful.DomHost      as DH
import           Reflex.Dom.Core               (TriggerEvent, PostBuild, PerformEvent, Prerender,
                                                MonadHold, DomBuilder, Performable)
import           Reflex.Spider.Internal        (SpiderTimeline, Global)
import           Control.Monad.Fix             (MonadFix)
import           Control.Monad.IO.Class        (MonadIO)

-- Mirrors upstream reflex-dom-core's 'TestWidget' constraint bundle
-- verbatim (see test/hydration.hs:1733).
type TestWidget t m =
  ( DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , Prerender t m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadFix m
  , MonadIO (Performable m)
  , MonadIO m
  )

-- The reproducer: pass a polymorphic TestWidget body through the port's
-- rank-N HydrationWidget slot. If this typechecks, the WidgetEff MTL
-- surface satisfies TestWidget under HydrationWidgetE — which is what
-- 'attachHydrationWidgetWithFailure' demands at line 5253 of DomHost.hs.
smokeAttach :: (forall x es. DH.HydrationWidget x es) -> IO ()
smokeAttach _ = pure ()

-- Body used to instantiate the rank-N slot. Empty () — the payload is
-- irrelevant, only the constraint plumbing matters.
emptyBody :: forall m. TestWidget (SpiderTimeline Global) m => m ()
emptyBody = pure ()

main :: IO ()
main = do
  smokeAttach emptyBody
  putStrLn "attach-hydration polymorphic-instantiation smoke OK."
