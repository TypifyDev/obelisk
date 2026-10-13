{-# LANGUAGE TupleSections #-}

module Jenga.Frontend.TimeExtras where

import Reflex hiding (PerformEvent, TriggerEvent, PostBuild, performEvent, holdDyn, foldDyn, getPostBuild)
import qualified Reflex.PerformEvent.Class as RPE (PerformEvent, performEvent)
import qualified Reflex.Class as RC (MonadHold)
import qualified Reflex.TriggerEvent.Class as RTE (TriggerEvent)
import qualified Reflex.PostBuild.Class as RPB (PostBuild, getPostBuild)
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Data.Time

import Effectful (Eff, (:>), IOE)
import Reflex.Class (holdDyn)
import Reflex.Dynamic (foldDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PerformEventH, performEventH)
import Reflex.Host.HeadlessEffectful (TriggerEvent)
import Control.Monad.IO.Class (liftIO, MonadIO)

timerE :: ( PerformEventH t :> es
          , Hold t :> es, Sample t :> es
          , TriggerEvent t :> es
          , IOE :> es
          , Reflex t
          , RPE.PerformEvent t (Eff es)
          , RC.MonadHold t (Eff es)
          , RTE.TriggerEvent t (Eff es)
          , MonadIO (Performable (Eff es))
          )
       => Event t ()
       -> Event t ()
       -> Event t ()
       -> Eff es (Dynamic t NominalDiffTime)
timerE start stop reset = do
  startTimeEv <- performEventH $ liftIO getCurrentTime <$ start
  mStartTime <- holdDyn Nothing $ leftmost
    [ Just <$> startTimeEv
    , Nothing <$ stop
    , Nothing <$ reset
    ]
  firstStart <- headE startTimeEv
  tick <- tickLossyFrom' $ (0.1,) <$> firstStart
  let elapsed = attachWith (\mStart tickInfo ->
        case mStart of
          Nothing -> 0
          Just s  -> diffUTCTime (_tickInfo_lastUTC tickInfo) s
        ) (current mStartTime) tick
  holdDyn 0 elapsed

-- | MTL twin of 'timerE'.
timer :: ( Reflex t
         , RPE.PerformEvent t m
         , RTE.TriggerEvent t m
         , RC.MonadHold t m
         , MonadIO (Performable m)
         , MonadFix m
         )
      => Event t ()
      -> Event t ()
      -> Event t ()
      -> m (Dynamic t NominalDiffTime)
timer start stop reset = do
  startTimeEv <- RPE.performEvent $ liftIO getCurrentTime <$ start
  mStartTime <- holdDyn Nothing $ leftmost
    [ Just <$> startTimeEv
    , Nothing <$ stop
    , Nothing <$ reset
    ]
  firstStart <- headE startTimeEv
  tick <- tickLossyFrom' $ (0.1,) <$> firstStart
  let elapsed = attachWith (\mStart tickInfo ->
        case mStart of
          Nothing -> 0
          Just s  -> diffUTCTime (_tickInfo_lastUTC tickInfo) s
        ) (current mStartTime) tick
  holdDyn 0 elapsed

tickLossyFrom'' :: ( PerformEventH t :> es
                   , TriggerEvent t :> es
                   , IOE :> es
                   , Reflex t
                   , RPE.PerformEvent t (Eff es)
                   , RTE.TriggerEvent t (Eff es)
                   , MonadIO (Performable (Eff es))
                   ) => NominalDiffTime -> Event t a -> Eff es (Event t TickInfo)
tickLossyFrom'' nomnom ev = do
  eventTime <- performEventH $ liftIO getCurrentTime <$ ev
  tickLossyFrom' $ (nomnom,) <$> eventTime

countTimeFromE :: ( PerformEventH t :> es
                  , TriggerEvent t :> es
                  , Hold t :> es, Sample t :> es
                  , IOE :> es
                  , Reflex t
                  , RPE.PerformEvent t (Eff es)
                  , RTE.TriggerEvent t (Eff es)
                  , MonadIO (Performable (Eff es))
                  ) => NominalDiffTime -> Event t a -> Eff es (Event t NominalDiffTime)
countTimeFromE interval ev = do
  eventTime <- performEventH $ liftIO getCurrentTime <$ ev
  eventTimeDyn <- foldDyn const undefined eventTime
  tick <- tickLossyFrom' $ (interval,) <$> eventTime
  pure $ attachWith (\start' now_ -> diffUTCTime (_tickInfo_lastUTC now_) start') (current eventTimeDyn) tick

-- | MTL twin of 'countTimeFromE'. Mirrors the Eff body using Reflex's MTL
-- classes directly (no 'Eff' wrapping needed; this function is polymorphic
-- in the host monad 'm').
countTimeFrom :: ( Reflex t
                 , RPE.PerformEvent t m
                 , RTE.TriggerEvent t m
                 , RC.MonadHold t m
                 , MonadIO (Performable m)
                 , MonadFix m
                 ) => NominalDiffTime -> Event t a -> m (Event t NominalDiffTime)
countTimeFrom interval ev = do
  eventTime <- RPE.performEvent $ liftIO getCurrentTime <$ ev
  eventTimeDyn <- foldDyn const (error "countTimeFrom: sampled before first event") eventTime
  tick <- tickLossyFrom' $ (interval,) <$> eventTime
  pure $ attachWith (\start' now_ -> diffUTCTime (_tickInfo_lastUTC now_) start') (current eventTimeDyn) tick

hasBeen :: Reflex t => NominalDiffTime -> Dynamic t NominalDiffTime -> Event t ()
hasBeen threshold tElapsed = mapMaybe (\t_ -> if t_ > threshold then Just () else Nothing) (updated tElapsed)

hasBeenRange :: Reflex t => (NominalDiffTime, NominalDiffTime) -> Dynamic t NominalDiffTime -> Event t ()
hasBeenRange (low, high) tElapsed = mapMaybe (\t_ -> if t_ > low && t_ < high then Just () else Nothing) (updated tElapsed)

getPostBuildDelayed
  :: ( Reflex t
     , RPB.PostBuild t m
     , RPE.PerformEvent t m
     , RC.MonadHold t m
     , RTE.TriggerEvent t m
     , MonadIO (Performable m)
     )
  => [NominalDiffTime]
  -> m (Event t ())
getPostBuildDelayed renderTimes = do
  pBuild <- RPB.getPostBuild
  pbDs <- forM renderTimes $ \t_ -> do
    delay t_ pBuild
  fmap (() <$) $ headE $ leftmost pbDs
