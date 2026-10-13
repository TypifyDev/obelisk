{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Auth.Login where

import Jenga.Frontend.Api (runAPIWithHeaders, runAPIWithHeadersMTL, RunAPI)
import Jenga.Frontend.Platform
import Jenga.Common.HasJengaConfig
import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Route.Servant (HasRoute)
import qualified Jenga.Route.Frontend as RouteE (SetRoute, setRoute)
import qualified Jenga.Route.FrontendMTL as RouteM (SetRoute, setRoute)
import Jenga.Route (R)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, leftmost)
import qualified Reflex.Class as R
import qualified Reflex.Dom.Core as RDC
import Reflex.Dom.Core (HasValue(..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Language.Javascript.JSaddle.Types (MonadJSM)

import qualified Data.Map as M
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Reflex.Dom.Core (InputElement, EventResult)
import Reflex.Effectful.DomHost (HasDocumentE)
import Reflex.Class (holdDyn)

data LoginConfig t = LoginConfig
  { _loginConfig_errors :: Dynamic t (Maybe T.Text)
  , _loginConfig_submitEnabled :: Dynamic t Bool
  , _loginConfig_token :: Event t (AuthToken, UserType)
  }

data LoginData s t m = LoginData
  { _login_username :: InputElement EventResult s t
  , _login_password :: InputElement EventResult s t
  , _login_submit :: Event t ()
  , _login_forgotPassword :: Event t ()
  , _login_goSignup :: Event t ()
  }

-- | Effectful variant. See 'login_FRP' for the MTL twin.
login_FRPE
  :: forall api backendR cfg frontendR t space es.
     ( RunAPI t space es
     , HasDocumentE space :> es
     , RouteE.SetRoute t frontendR :> es
     , HasRoute api backendR
     , Reader cfg :> es, HasConfig cfg BaseURL
     )
  => backendR
  -> frontendR
  -> frontendR
  -> LoginData space t (Eff es)
  -> Eff es (LoginConfig t)
login_FRPE loginRoute requestNewPasswordPageRoute signupRoute (LoginData user pass submit forgotPass goSignup) = do
  let credentials :: Event t (T.Text, T.Text)
      credentials = tag (current $ (,) <$> value user <*> value pass) submit
  (err :: Event t (RequestError LoginError), token :: Event t (AuthToken, UserType)) <-
    runAPIWithHeaders @api @backendR @t @space @es @cfg loginRoute (M.fromList [(clientTypeHeader, T.pack . show $ clientType)]) credentials
  shouldSubmit <- holdDyn True $ leftmost [False <$ credentials, True <$ token, True <$ err]
  errors <- holdDyn Nothing $ Just . showUser <$> err
  RouteE.setRoute $ requestNewPasswordPageRoute <$ forgotPass
  RouteE.setRoute $ signupRoute <$ goSignup
  pure LoginConfig
    { _loginConfig_errors = errors
    , _loginConfig_submitEnabled = shouldSubmit
    , _loginConfig_token = token
    }

-- | MTL twin of 'login_FRPE'. Calls 'runAPIWithHeadersMTL' which already
-- handles the client-side XHR + decoding + fanning; this function just
-- builds the credentials tuple from the two 'InputElement' values,
-- threads error/success events, and routes on forgot-password / signup.
login_FRP
  :: forall be fe cfg frontendR t m.
     ( RDC.DomBuilder t m, RDC.Prerender t m, RDC.MonadHold t m
     , RDC.PerformEvent t (RDC.Client m), RDC.TriggerEvent t (RDC.Client m)
     , MonadIO (RDC.Performable (RDC.Client m))
     , MonadJSM (RDC.Performable (RDC.Client m))
     , MonadReader cfg m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , RouteM.SetRoute t frontendR m
     , Reflex t
     )
  => R be
  -> frontendR
  -> frontendR
  -> LoginData (RDC.DomBuilderSpace m) t m
  -> m (LoginConfig t)
login_FRP loginRoute requestNewPasswordPageRoute signupRoute (LoginData user pass submit forgotPass goSignup) = do
  let credentials :: Event t (T.Text, T.Text)
      credentials = tag (current $ (,) <$> value user <*> value pass) submit
  (err, token) <-
    runAPIWithHeadersMTL @be @fe @t @m @cfg @(T.Text, T.Text) @(AuthToken, UserType) @LoginError
      loginRoute
      (M.fromList [(clientTypeHeader, T.pack . show $ clientType)])
      credentials
  shouldSubmit <- R.holdDyn True $ leftmost [False <$ credentials, True <$ token, True <$ err]
  errors <- R.holdDyn Nothing $ Just . showUser <$> err
  RouteM.setRoute $ requestNewPasswordPageRoute <$ forgotPass
  RouteM.setRoute $ signupRoute <$ goSignup
  pure LoginConfig
    { _loginConfig_errors = errors
    , _loginConfig_submitEnabled = shouldSubmit
    , _loginConfig_token = token
    }
