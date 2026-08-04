{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.Subscribe.StartFreeTrial  where

import Jenga.Common.Auth
import Jenga.Common.Errors

import Jenga.Route.Frontend (Routed, askRoute)
import Reflex (Reflex, Event, Dynamic, current, tag, attach, leftmost, ffor, fanEither)

import qualified Text.Email.Validate as EmailValidate
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Either (isRight)

import Templates.Types ()

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.Prerender (Prerender)

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

startFreeTrial_FRP
  :: forall req rsp t es.
     ( Routed t (Map.Map T.Text (Maybe T.Text)) :> es
     , Dom t :> es
     , Hold t :> es
     , PostBuild t :> es
     , Prerender t :> es
     , Reflex t
     , KnownTimeline es t
     )
  => ((Maybe T.Text, Email) -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> StartFreeTrialData t
  -> Eff es (StartFreeTrialConfig t)
startFreeTrial_FRP mkAPI_NewFreeTrial sendRequest (StartFreeTrialData email confirm clickSubscribe) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- askRoute
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
  let (errRes, good) = fanEither (castResponse response) :: (Event t FreeTrialError, Event t ())
  errors <- holdDyn "" $ leftmost
    [ showUser <$> errRes
    , emailErr
    ]
  pure $ StartFreeTrialConfig code errors good
  where
    castResponse = undefined -- TODO: cast response type
