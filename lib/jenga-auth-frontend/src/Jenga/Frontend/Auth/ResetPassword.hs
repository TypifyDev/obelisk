{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Auth.ResetPassword where

import Jenga.Frontend.Api (runAPIEff)
import Jenga.Common.HasJengaConfig (BaseURL, HasConfig)
import Jenga.Common.Auth
import Jenga.Common.Errors
import Templates.Types
import Jenga.Route (HasRoute)
import Jenga.Route.Frontend (Routed, askRoute)
import Reflex (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)
import Reflex.Dom.Core (HasValue(..))

import Data.Proxy (Proxy)
import Data.Signed (Signed)
import qualified Data.Text as T
import Data.Text (Text)
import Rhyolite.Account (PasswordResetToken)

import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent)
import Reflex.Effectful.Effect.Prerender (Prerender)
import Jenga.Frontend.Effectful.Configs (Configs)

data ResetPasswordConfig t = ResetPasswordConfig
  { _resetPasswordConfig_errors :: Dynamic t (Maybe Text)
  , _resetPasswordConfig_result :: Event t (AuthToken, UserType)
  }

data ResetPasswordData t m = ResetPasswordData
  { _resetPassword_password :: InputEl t m
  , _resetPassword_confirmPassword :: InputEl t m
  , _resetPassword_submit :: Event t ()
  }

resetPassword_FRP ::
  forall api backendR cfg t es.
  ( Hold t :> es
  , Dom t :> es
  , PostBuild t :> es
  , Routed t (Signed PasswordResetToken) :> es
  , Configs :> es
  , Prerender t :> es
  , PerformEvent t :> es
  , TriggerEvent t :> es
  , HasRoute api backendR
  , Reader cfg :> es, HasConfig cfg BaseURL
  , Reflex t
  , KnownTimeline es t
  )
  => Proxy api
  -> backendR
  -> ( ResetPasswordData t (Eff es) )
  -> Eff es (ResetPasswordConfig t)
resetPassword_FRP proxy routeReset (ResetPasswordData pass conf submit) = do
  token :: Dynamic t (Signed PasswordResetToken) <- askRoute
  let credentials = tag (current $ (,,) <$> token <*> value pass <*> value conf) submit
      (credError, validCredentials) = fanEither $ ffor credentials $ \(tok, pw, cf) ->
        case (pw == cf, T.null pw) of
          (True, False) -> Right (tok, pw)
          (False, False) -> Left "Passwords don't match"
          (_, True) -> Left "Empty fields"
  (err :: Event t (RequestError ResetPasswordError), result :: Event t (AuthToken, UserType)) <-
    runAPIEff @api @backendR @t @es @cfg proxy routeReset validCredentials
  errors <- holdDyn Nothing $ leftmost $ fmap (Just <$>) [credError, showUser <$> err]
  pure $ ResetPasswordConfig errors result
