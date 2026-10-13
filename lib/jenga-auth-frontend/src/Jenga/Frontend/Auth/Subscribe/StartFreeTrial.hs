{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.Subscribe.StartFreeTrial  where

import Jenga.Common.Auth
import Jenga.Common.Errors

import qualified Jenga.Route.Frontend as RouteE (Routed, askRoute)
import qualified Jenga.Route.FrontendMTL as RouteM (Routed, askRoute)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, attach, leftmost, ffor, fanEither)
import qualified Reflex.Class as R

import qualified Text.Email.Validate as EmailValidate
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Either (isRight)


import Effectful (Eff, (:>))
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PostBuild)

--(email, confirm, clickSubscribe) <- startFreeTrial_TMPL (code, errors)
data StartFreeTrialData t = StartFreeTrialData
  { _startFreeTrialData_email :: Dynamic t T.Text
  , _startFreeTrialData_confirm :: Dynamic t T.Text
  , _startFreeTrialData_clickSubscribe :: Event t ()
  }
--(Dynamic t T.Text, Dynamic t T.Text, Event t ())
data StartFreeTrialConfig t = StartFreeTrialConfig
  { _startFreeTrialConfig_code :: Dynamic t (Maybe T.Text)
  , _startFreeTrialConfig_errors :: Dynamic t T.Text
  , _startFreeTrialConfig_success :: Event t ()
  }

-- | Widened error type: this family now accepts the full
-- 'BackendError FreeTrialError' surface, so every backend branch
-- (BUserError, BCritical, NoAuth family, BException, BInvalidRequest)
-- reaches the user via the 'ShowUser (BackendError e)' instance. This
-- eliminates the systemic 'fmapMaybe'-drop workarounds that used to
-- live in each consumer, which silently swallowed non-inner-error
-- branches.
--
-- Depinned: this function is pure FRP wiring — it consumes the already-
-- built 'StartFreeTrialData' record, plumbs it through 'sendRequest',
-- and returns a 'StartFreeTrialConfig'. It never constructs DOM, so it
-- has no 'DomBuilderE' / 'DomBuilderSpaceE' / 'HasPrerender' constraint,
-- and consumers can call it from any effect row (immediate / static /
-- hydration / prerender). The template widget that builds the actual
-- 'InputElement's owns those pins, not this FRP function.
-- | Effectful form. See 'startFreeTrial_FRP' for the MTL twin.
startFreeTrial_FRPE
  :: forall req rsp t es.
     ( RouteE.Routed t (Map.Map T.Text (Maybe T.Text)) :> es
     , Hold t :> es, Sample t :> es
     , PostBuild t :> es
     , Reflex t

     )
  => ((Maybe T.Text, Email) -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError FreeTrialError) ())
  -> StartFreeTrialData t
  -> Eff es (StartFreeTrialConfig t)
startFreeTrial_FRPE mkAPI_NewFreeTrial sendRequest castResponse (StartFreeTrialData email confirm clickSubscribe) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- RouteE.askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let
    confirmedEmailF e conf =
      if e /= conf
      then Left "Emails don't match"
      else
        if (isRight $ EmailValidate.validate . T.encodeUtf8 $ e)
        then Right $ Email e
        else Left "Invalid Email"
    (emailErr, emailsGood) = fanEither $ tag (current $ confirmedEmailF <$> email <*> confirm) clickSubscribe
  let req = ffor (attach (current code) emailsGood) mkAPI_NewFreeTrial

  response <- sendRequest req
  let (errRes, good) = fanEither (fmap castResponse response) :: (Event t (BackendError FreeTrialError), Event t ())
  -- 'showUser' on 'BackendError e' renders every branch; nothing is
  -- dropped.
  errors <- holdDyn "" $ leftmost
    [ showUser <$> errRes
    , emailErr
    ]
  pure $ StartFreeTrialConfig code errors good

-- | MTL twin of 'startFreeTrial_FRPE'. Pure FRP wiring: consumes the
-- already-built 'StartFreeTrialData' record, plumbs it through the
-- caller-supplied 'sendRequest' (which itself runs in @m@), and returns
-- a 'StartFreeTrialConfig'. No DOM, so no 'DomBuilder' / 'Prerender'
-- constraints — any Reflex MTL monad with 'MonadHold' and 'Routed'
-- works.
--
-- Body mirrors 'startFreeTrial_FRPE' verbatim; only the Eff-vs-MTL
-- plumbing (askRoute, sendRequest return type) differs.
startFreeTrial_FRP
  :: forall req rsp t m.
     ( RouteM.Routed t (Map.Map T.Text (Maybe T.Text)) m
     , R.MonadHold t m
     , Reflex t
     )
  => ((Maybe T.Text, Email) -> req)
  -> (Event t req -> m (Event t rsp))
  -> (rsp -> Either (BackendError FreeTrialError) ())
  -> StartFreeTrialData t
  -> m (StartFreeTrialConfig t)
startFreeTrial_FRP mkAPI_NewFreeTrial sendRequest castResponse (StartFreeTrialData email confirm clickSubscribe) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- RouteM.askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let
    confirmedEmailF e conf =
      if e /= conf
      then Left "Emails don't match"
      else
        if (isRight $ EmailValidate.validate . T.encodeUtf8 $ e)
        then Right $ Email e
        else Left "Invalid Email"
    (emailErr, emailsGood) = fanEither $ tag (current $ confirmedEmailF <$> email <*> confirm) clickSubscribe
  let req = ffor (attach (current code) emailsGood) mkAPI_NewFreeTrial

  response <- sendRequest req
  let (errRes, good) = fanEither (fmap castResponse response) :: (Event t (BackendError FreeTrialError), Event t ())
  -- 'showUser' on 'BackendError e' renders every branch; nothing is
  -- dropped.
  errors <- R.holdDyn "" $ leftmost
    [ showUser <$> errRes
    , emailErr
    ]
  pure $ StartFreeTrialConfig code errors good
