{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Env + state types + effect declarations for the Effectful-native port
-- of @Reflex.Dom.Builder.Immediate@.
--
-- The effect DATA declarations for 'JSM'' and 'DomRenderHook' live here
-- (not in their interpreter modules) so 'HydrationEnv' can reference
-- them in the delayed-runner storage type without forming an import
-- cycle. The interpreters — 'runJSM'' in
-- "Reflex.Dom.Core" and 'runDomRenderHook'
-- in "Reflex.Dom.Core" — live in
-- those dedicated modules along with the send-wrappers.
--
-- Native port design (contrast with upstream MTL):
--
--   * The upstream monolith uses a
--     @HydrationDomBuilderT s t m@ = @ReaderT (HydrationDomBuilderEnv t m)
--     (DomRenderHookT t m)@ transformer newtype, and a
--     @HydrationRunnerT t m@ = @StateT HydrationState (ReaderT Node
--     (DomRenderHookT t m))@ transformer.
--
--   * In the Effectful port those transformer stacks are decomposed into
--     an effect row:
--
--       - The Reader-of-'HydrationEnv' becomes @Reader (HydrationEnv t) :> es@
--       - The State-of-'HydrationState' (only present in the runner scope)
--         becomes @State HydrationState :> es@
--       - The Reader-of-parent-'Node' (only in the runner scope) becomes
--         @Reader Node :> es@
--       - The 'DomRenderHookT'-embedded @RequesterT ... (TriggerEventT ...)@
--         is replaced by the dedicated 'DomRenderHook' effect.
--
-- The end result: every widget is @Eff es ()@ with a specific effect row,
-- there is no @m@-parameter anywhere in the env types, and the constraint
-- synonyms 'DomE' / 'HydrationRunnerE' name the row required by a given
-- scope.
--
-- The 'RunnerAction' newtype wraps a rank-N 'Eff' body polymorphic in
-- its effect row (constrained by 'HydrationRunnerE'), so delayed
-- runner actions can be stored in an 'IORef' (as 'DelayedHydration')
-- and specialised at drain time. Upstream stores
-- @HydrationRunnerT t m ()@ in an 'IORef' and gets the same
-- late-binding via @lift@; the port gets it via row-polymorphism.
module Reflex.Effectful.Dom.Builder.Immediate.Types
  ( -- * State + mode (re-exported from DomHost)
    HydrationState (..)
  , HydrationMode (..)
  , HydrationEnv (..)
  , HydrationRunnerE
  , RunnerAction (..)
  , DelayedHydration
    -- * Effects (data decls; interpreters live in dedicated modules)
  , JSM' (..)
  , DomRenderHook (..)
    -- * Constraint synonyms
  , DomE
    -- * Send-wrappers
  , askJSM
  , liftJSM
    -- * JSM + PerformEvent combinators
  , performJSMEvent_
  , performJSMEvent
  ) where

import           Control.Concurrent.Chan                 (Chan)
import           Data.Dependent.Sum                      (DSum)
import           Data.IORef                              (IORef)
import           Data.Kind                               (Type)
import           Effectful                               (Dispatch (..),
                                                          DispatchOf, Eff,
                                                          Effect, IOE, (:>))
import           Effectful.Dispatch.Dynamic              (send)
import           JSDOM.Extras.JSM                        (JSM' (..), askJSM, liftJSM)
import           Control.Monad.IO.Class                  (liftIO)
import           Effectful.Reader.Static                 (Reader)
import           Effectful.State.Static.Local            (State)
import           GHCJS.DOM.Types                         (Document, JSM,
                                                          JSContextRef, Node,
                                                          runJSM)

import           Reflex.Class                  (Event, Reflex, ffor)
import           Reflex.Spider (Global)
import           Reflex.Host.HeadlessEffectful (EventTriggerRef, Adjustable, Hold, NotReady, PostBuild, Sample, TriggerEvent, TriggerInvocation, PerformEventH, performEventH, performEventH_)

-- Types + constraint synonyms relocated to Reflex.Effectful.DomHost.
-- Re-imported here and re-exported (see module header) so existing
-- consumers of this module keep working.
import           Reflex.Effectful.DomHost
                     ( HydrationState (..), HydrationMode (..)
                     , HydrationEnv (..), HydrationRunnerE
                     , RunnerAction (..), DelayedHydration
                     )

-- ---------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------

-- 'JSM'' + 'askJSM' + 'liftJSM' relocated to 'JSDOM.Extras.JSM' in the
-- jsaddle-dom-extras package (pure JSaddle-tier concept — no reflex-dom
-- coupling). Re-exported here for API stability.

-- | Fire a per-occurrence 'JSM' action on the render hook. Eliminates
-- the repeating boilerplate:
--
-- @
-- ctx <- askJSM
-- performEventH_ $ ffor evt $ \\a -> EventME (runJSM (jsmAction a) ctx)
-- @
--
-- The action runs against the ambient jsaddle 'JSContextRef' bound by
-- the surrounding 'runJSM'' interpreter. Fire-and-forget: the JSM's
-- unit result is discarded; use 'performJSMEvent' if you need the
-- returned value as a downstream 'Event'.
performJSMEvent_
  :: forall t es
   . (Reflex t, JSM' :> es, PerformEventH t :> es, IOE :> es)
  => Event t (JSM ())
  -> Eff es ()
performJSMEvent_ evt = do
  ctx <- askJSM
  performEventH_ $ ffor evt (\jsm -> liftIO (runJSM jsm ctx))

-- | Value-returning variant of 'performJSMEvent_'. Fires a 'JSM'
-- action per occurrence of the carrier event and delivers each result
-- as an occurrence of the returned 'Event'.
--
-- Same ambient-context semantics as 'performJSMEvent_'; the JSM action
-- runs against the 'JSContextRef' bound by the surrounding 'runJSM''
-- interpreter.
performJSMEvent
  :: forall t es a
   . (Reflex t, JSM' :> es, PerformEventH t :> es, IOE :> es)
  => Event t (JSM a)
  -> Eff es (Event t a)
performJSMEvent evt = do
  ctx <- askJSM
  performEventH $ ffor evt (\jsm -> liftIO (runJSM jsm ctx))

-- | Effect replacing the @DomRenderHookT@ transformer. The three
-- operations mirror the class from
-- "Reflex.Dom.Core":
--
--   * 'WithRenderHook' wraps every JSM action fired by subsequently
--     nested 'RequestDomAction' calls with the supplied transform. Used
--     e.g. by prerender to run subordinate JSM in a batched frame.
--   * 'RequestDomAction' schedules an 'Event'-driven JSM action and
--     hands back the JSM's result as an 'Event'.
--   * 'RequestDomAction_' is the fire-and-forget variant.
data DomRenderHook (t :: Type) :: Effect where
  WithRenderHook    :: (forall x. JSM x -> JSM x) -> m a -> DomRenderHook t m a
  RequestDomAction  :: Event t (JSM a) -> DomRenderHook t m (Event t a)
  RequestDomAction_ :: Event t (JSM ()) -> DomRenderHook t m ()

type instance DispatchOf (DomRenderHook t) = 'Dynamic

-- ---------------------------------------------------------------------
-- Hydration mode + state
-- ---------------------------------------------------------------------

-- | Whether the builder is in the initial hydration pass (matching a
-- statically prerendered DOM) or the post-hydration steady-state where
-- new nodes are appended immediately.
-- HydrationMode relocated to Reflex.Effectful.DomHost (authoritative).
-- Re-exported below via the module header.

-- | State threaded through 'HydrationRunner' actions.
--
-- The runner walks the prerendered DOM left-to-right, and needs to
-- remember (a) which node it last visited so the next sibling lookup
-- works, and (b) whether hydration has already diverged from the
-- expected DOM shape (in which case the runner emits a warning at
-- switchover).
-- HydrationState relocated to Reflex.Effectful.DomHost (authoritative;
-- includes a '_hydrationState_failures :: ![HydrationFailure]' field
-- for structured divergence diagnostics).

-- ---------------------------------------------------------------------
-- Env
-- ---------------------------------------------------------------------

-- | Ambient env for the 'DomE' scope.
--
-- The parent-node reference is an 'Either' because during the
-- hydration pass the true parent is not known until the runner
-- reaches this subtree; the 'IORef' is populated by the runner and
-- the 'Left' branch is used post-hydration where the parent is fixed.
--
-- The 'HydrationRunner' 'IORef' collects delayed actions that were
-- queued via 'addHydrationStep'; the outer driver runs them
-- sequentially on the switchover event.
-- HydrationEnv relocated to Reflex.Effectful.DomHost (authoritative).

-- ---------------------------------------------------------------------
-- Constraint synonyms
-- ---------------------------------------------------------------------

-- | The full effect row required by widget-scope code (element
-- construction, attribute updates, event registration, etc).
--
-- Broken down:
--
--   * @Reader (HydrationEnv t) :> es@ — ambient DOM env (document,
--     parent, hydration mode, ...).
--   * @DomRenderHook t :> es@ — schedules JSM actions on the render
--     hook (used by 'requestDomAction' and 'withRenderHook').
--   * @PostBuild t :> es@ / @PerformEvent :> es@ / @TriggerEvent :> es@
--     — the standard FRP effect trio.
--   * @NotReady :> es@ — used by widgets that want to defer
--     rendering (mount stall).
--   * @JSM' :> es@ — access to the underlying jsaddle context.
--   * @IOE :> es@ — 'IORef', 'Chan', etc.
--
-- The @s@ type-parameter is the DOM space (currently only
-- 'Reflex.Dom.Core' but the
-- upstream layout also supports @HydrationDomSpace@); the port keeps
-- the phantom so we don't collapse two semantically distinct spaces
-- into one.
type DomE (t :: Type) (s :: Type) es =
  ( Reader (HydrationEnv t) :> es
  , DomRenderHook t :> es
  , PostBuild t :> es
  , PerformEventH t :> es
  , TriggerEvent t :> es
  , NotReady t :> es
  , Hold t :> es
  , Sample t :> es
  , Adjustable t :> es
  , JSM' :> es
  , IOE :> es
  )

-- | The effect row required by runner-scope code (delayed hydration
-- actions).
--
-- Compared to 'DomE' the widget-scope 'Reader (HydrationEnv t)' is
-- replaced by @State HydrationState :> es@ (cursor state) and
-- @Reader Node :> es@ (immediate parent node); the FRP + JSM
-- constraints are otherwise identical.
--
-- Widened (vs. the earlier narrow shortcut) to include EVERY effect a
-- runner-scope action might need — the same ones a widget-scope body
-- can call. Rationale: upstream reflex-dom's @HydrationRunnerT t m@
-- newtype gives runner-scope code full access to the widget's @m@ via
-- @lift@; effect rows can't nest that way, so the runner row instead
-- carries the full widget capability set directly. The prerender
-- hydration instance (@HasPrerender t HydrationDomSpace@) queues a
-- client-widget body that needs 'PostBuild', 'Adjustable', 'NotReady',
-- and 'Reader (HydrationEnv t)' inside the runner scope; those all
-- appear here.
-- HydrationRunnerE, RunnerAction, DelayedHydration all relocated to
-- Reflex.Effectful.DomHost (authoritative). DomHost's HydrationRunnerE
-- is decomposed into HydrationRunnerE + HydrationRunnerOuterE with a
-- slightly different constraint set (uses DomRenderHookE, HH.-qualified
-- PostBuild/Adjustable/etc.). Consumers should import from DomHost.
