module Reflex.Effectful.Run
  ( -- * Constraint aliases (t inferred via KnownTimeline fundep)
    FRP'
  , DomEff'
  , InteractiveEff'
  , DomAccessEff'
  , WidgetEff'

    -- * Explicit-t constraint aliases
  , FRP, DomEff, InteractiveEff, DomAccessEff, WidgetEff

    -- * Entry point
  , mainWidgetEff
  ) where

import           Effectful                (IOE, (:>))
import           Reflex                   (Reflex)

import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex.Effectful.Effect.Sample
import           Reflex.Effectful.Effect.Hold
import           Reflex.Effectful.Effect.PostBuild
import           Reflex.Effectful.Effect.TriggerEvent
import           Reflex.Effectful.Effect.PerformEvent
import           Reflex.Effectful.Effect.Adjustable
import           Reflex.Effectful.Effect.Dom
import           Reflex.Effectful.Effect.Prerender
import           Reflex.Effectful.Effect.JSM
import           Reflex.Effectful.Effect.HasDocument
import           Reflex.Effectful.Effect.DomRenderHook
import           Reflex.Effectful.Effect.NotReady

import           Reflex.Effectful.Internal.Main (mainWidgetEff)

-- ─── Explicit-t aliases ────────────────────────────────────────

type FRP t es = (Hold t :> es, Sample t :> es)

type DomEff t es =
  ( Dom t :> es, FRP t es, PostBuild t :> es, NotReady t :> es )

type InteractiveEff t es =
  ( DomEff t es, PerformEvent t :> es, TriggerEvent t :> es
  , JSM' :> es, IOE :> es )

type DomAccessEff t es =
  ( InteractiveEff t es, HasDocument :> es, DomRenderHook t :> es )

type WidgetEff t es =
  ( DomAccessEff t es, Adjustable t :> es, Prerender t :> es )

-- ─── Fundep aliases (t determined from es, no @t needed) ───────

type FRP' es t = (KnownTimeline es t, FRP t es, Reflex t)

type DomEff' es t = (KnownTimeline es t, DomEff t es, Reflex t)

type InteractiveEff' es t = (KnownTimeline es t, InteractiveEff t es, Reflex t)

type DomAccessEff' es t = (KnownTimeline es t, DomAccessEff t es, Reflex t)

type WidgetEff' es t = (KnownTimeline es t, WidgetEff t es, Reflex t)
