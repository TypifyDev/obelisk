{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Auth.Login where

import Jenga.Frontend.Api (runAPIWithHeaders)
import Jenga.Frontend.Platform
import Jenga.Common.HasJengaConfig
import Templates.Types
import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Route (HasRoute)
import Jenga.Route.Frontend (SetRoute, setRoute)
import Reflex (Reflex, Event, Dynamic, current, tag, leftmost)

import Data.Proxy (Proxy)
import qualified Data.Map as M
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Effectful.Reader.Static ()
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent)
import Reflex.Effectful.Effect.Prerender (Prerender)
import Jenga.Frontend.Effectful.Configs (Configs)

data LoginConfig t = LoginConfig
  { _loginConfig_errors :: Dynamic t (Maybe T.Text)
  , _loginConfig_submitEnabled :: Dynamic t Bool
  , _loginConfig_token :: Event t (AuthToken, UserType)
  }

data LoginData t m = LoginData
  { _login_username :: InputEl t m
  , _login_password :: InputEl t m
  , _login_submit :: Event t ()
  , _login_forgotPassword :: Event t ()
  , _login_goSignup :: Event t ()
  }

login_FRP
  :: forall api backendR cfg frontendR t es.
     ( Hold t :> es
     , Dom t :> es
     , PostBuild t :> es
     , SetRoute t frontendR :> es
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
  -> frontendR
  -> frontendR
  -> LoginData t (Eff es)
  -> Eff es (LoginConfig t)
login_FRP proxy loginRoute requestNewPasswordPageRoute signupRoute (LoginData user pass submit forgotPass goSignup) = do
  let credentials :: Event t (T.Text, T.Text)
      credentials = tag (current $ (,) <$> value user <*> value pass) submit
  (err :: Event t (RequestError LoginError), token :: Event t (AuthToken, UserType)) <-
    runAPIWithHeaders @api @backendR @t @es @cfg proxy loginRoute (M.fromList [(clientTypeHeader, T.pack . show $ clientType)]) credentials
  shouldSubmit <- holdDyn True $ leftmost [False <$ credentials, True <$ token, True <$ err]
  errors <- holdDyn Nothing $ Just . showUser <$> err
  setRoute $ requestNewPasswordPageRoute <$ forgotPass
  setRoute $ signupRoute <$ goSignup
  pure LoginConfig
    { _loginConfig_errors = errors
    , _loginConfig_submitEnabled = shouldSubmit
    , _loginConfig_token = token
    }
  where value = undefined -- TODO: from Templates.Types InputEl accessor
