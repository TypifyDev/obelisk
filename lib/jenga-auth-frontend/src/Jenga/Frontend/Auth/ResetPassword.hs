{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jenga.Frontend.Auth.ResetPassword where

import Jenga.Common.Auth
import Jenga.Common.Errors
import qualified Jenga.Route.Frontend as RouteE (Routed, askRoute)
import qualified Jenga.Route.FrontendMTL as RouteM (Routed, askRoute)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)
import qualified Reflex.Class as R
import Reflex.Dom.Core (HasValue(..))

import Data.Signed (Signed)
import qualified Data.Text as T
import Data.Text (Text)
import Rhyolite.Account (PasswordResetToken)

import Effectful (Eff, (:>))
import Reflex.Dom.Core (InputElement, EventResult, DomBuilderSpace)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PostBuild)

data ResetPasswordConfig t = ResetPasswordConfig
  { _resetPasswordConfig_errors :: Dynamic t (Maybe Text)
  , _resetPasswordConfig_result :: Event t (AuthToken, UserType)
  }

-- | Parameterized on the DOM space 's' so consumers thread whatever
-- 'DomBuilderSpaceE es' resolves to in their effect row, instead of the
-- library hardcoding 'GhcjsDomSpace'. This lets the same record work
-- against static / hydration / immediate / prerender spaces.
data ResetPasswordData s t = ResetPasswordData
  { _resetPassword_password :: InputElement EventResult s t
  , _resetPassword_confirmPassword :: InputElement EventResult s t
  , _resetPassword_submit :: Event t ()
  }

-- | MTL twin alias: pins @s@ to the ambient @DomBuilderSpace m@.
type ResetPasswordDataMTL t m =
  ResetPasswordData (DomBuilderSpace m) t

-- | Runtime-injected variant matching the depin'd shape of the sibling
-- 'redeemLink_FRP' / 'requestPasswordReset_FRP' modules. Instead of the
-- library reaching into 'runAPI' directly (which pinned this module to
-- 'DomBuilderSpaceE es ~ GhcjsDomSpace' + 'JSM'' + 'IOE' + 'Configs' +
-- 'Reader cfg' + 'HasPrerender' + 'HasRoute'), the caller supplies three
-- pure hooks:
--
--   * @mkReq@       — build the concrete backend-request payload from
--                     the validated @(token, password)@ tuple.
--   * @sendRequest@ — the transport itself. Downstream typically wires
--                     this to @runAPI \@FrontendRoute \@BackendRoute@ or
--                     a mock in tests.
--   * @castResponse@ — project the raw response into
--                     @Either (BackendError ResetPasswordError) (AuthToken, UserType)@.
--
-- All JS / XHR / config / route-rendering concerns stay in the caller's
-- effect row, never in the library's signature. Every backend branch
-- (BUserError, BCritical, NoAuth family, BException, BInvalidRequest)
-- reaches the user via the 'ShowUser (BackendError e)' instance —
-- nothing is dropped.
-- | Effectful variant. See 'resetPassword_FRP' for the MTL twin.
resetPassword_FRPE
  :: forall req rsp s t es.
     ( Hold t :> es, Sample t :> es
     , PostBuild t :> es
     , RouteE.Routed t (Signed PasswordResetToken) :> es
     , Reflex t

     )
  => ((Signed PasswordResetToken, T.Text) -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError ResetPasswordError) (AuthToken, UserType))
  -> ResetPasswordData s t
  -> Eff es (ResetPasswordConfig t)
resetPassword_FRPE mkReq sendRequest castResponse (ResetPasswordData pass conf submit) = do
  token :: Dynamic t (Signed PasswordResetToken) <- RouteE.askRoute
  let credentials = tag (current $ (,,) <$> token <*> value pass <*> value conf) submit
      (credError, validCredentials) = fanEither $ ffor credentials $ \(tok, pw, cf) ->
        case (pw == cf, T.null pw) of
          (True, False) -> Right (tok, pw)
          (False, False) -> Left "Passwords don't match"
          (_, True) -> Left "Empty fields"
  let req = mkReq <$> validCredentials
  response <- sendRequest req
  let (err, result) =
        fanEither (castResponse <$> response)
        :: (Event t (BackendError ResetPasswordError), Event t (AuthToken, UserType))
  errors <- holdDyn Nothing $ leftmost $ fmap (fmap Just) [credError, showUser <$> err]
  pure $ ResetPasswordConfig errors result

-- | MTL twin of 'resetPassword_FRPE'.
resetPassword_FRP
  :: forall req rsp t m.
     ( R.MonadHold t m
     , RouteM.Routed t (Signed PasswordResetToken) m
     , Reflex t
     )
  => ((Signed PasswordResetToken, T.Text) -> req)
  -> (Event t req -> m (Event t rsp))
  -> (rsp -> Either (BackendError ResetPasswordError) (AuthToken, UserType))
  -> ResetPasswordDataMTL t m
  -> m (ResetPasswordConfig t)
resetPassword_FRP mkReq sendRequest castResponse (ResetPasswordData pass conf submit) = do
  token :: Dynamic t (Signed PasswordResetToken) <- RouteM.askRoute
  let credentials = tag (current $ (,,) <$> token <*> value pass <*> value conf) submit
      (credError, validCredentials) = fanEither $ ffor credentials $ \(tok, pw, cf) ->
        case (pw == cf, T.null pw) of
          (True, False) -> Right (tok, pw)
          (False, False) -> Left "Passwords don't match"
          (_, True) -> Left "Empty fields"
  let req = mkReq <$> validCredentials
  response <- sendRequest req
  let (err, result) =
        fanEither (castResponse <$> response)
        :: (Event t (BackendError ResetPasswordError), Event t (AuthToken, UserType))
  errors <- R.holdDyn Nothing $ leftmost $ fmap (fmap Just) [credError, showUser <$> err]
  pure $ ResetPasswordConfig errors result
