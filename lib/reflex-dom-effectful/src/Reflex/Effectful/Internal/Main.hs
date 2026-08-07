{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Effectful.Internal.Main (mainWidgetEff) where

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class   (liftIO)
import           Effectful                (Eff, IOE, runEff)

import           Reflex.Effectful.Types   (DomTimeline, Timeline, runTimeline)
import           Reflex.Effectful.Effect.Sample           (Sample)
import           Reflex.Effectful.Effect.Hold             (Hold)
import           Reflex.Effectful.Effect.PostBuild        (PostBuild)
import           Reflex.Effectful.Effect.TriggerEvent     (TriggerEvent)
import           Reflex.Effectful.Effect.PerformEvent     (PerformEvent)
import           Reflex.Effectful.Effect.Dom              (Dom)
import           Reflex.Effectful.Effect.Adjustable       (Adjustable)
import           Reflex.Effectful.Effect.Prerender        (Prerender)
import           Reflex.Effectful.Effect.JSM              (JSM')
import           Reflex.Effectful.Effect.HasDocument      (HasDocument)
import           Reflex.Effectful.Effect.DomRenderHook    (DomRenderHook)
import           Reflex.Effectful.Effect.NotReady         (NotReady)
import           Reflex.Effectful.Internal.Channel        (WidgetChannel, WidgetOp(..), WidgetReq(..), runWidgetChannel)
import           Reflex.Effectful.Internal.Interpret      (interpretAll)
import           Reflex.Effectful.Internal.WidgetLoop     (processLoop)

import qualified Reflex.Dom.Core          as RD
import           GHCJS.DOM.Types          (JSM)

mainWidgetEff
  :: Eff '[ Dom DomTimeline, Hold DomTimeline, Sample DomTimeline, PostBuild DomTimeline
          , PerformEvent DomTimeline, TriggerEvent DomTimeline
          , Adjustable DomTimeline, Prerender DomTimeline
          , JSM', HasDocument, DomRenderHook DomTimeline
          , NotReady DomTimeline
          , Timeline DomTimeline
          , WidgetChannel DomTimeline, IOE ] ()
  -> JSM ()
mainWidgetEff userEff = RD.mainWidget $ do
  chanVar <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ do
    runEff
      $ runWidgetChannel chanVar
      $ runTimeline
      $ interpretAll @DomTimeline
      $ userEff
    doneResp <- newEmptyMVar
    putMVar chanVar (WidgetReq WODone doneResp)
    takeMVar doneResp
  _ <- processLoop chanVar
  return ()
