{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RedeemLink where

import Jenga.Common.Errors
import Jenga.Common.Auth
import qualified Jenga.Route.Frontend as RouteE (Routed, askRoute)
import qualified Jenga.Route.FrontendMTL as RouteM (Routed, askRoute)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, attach, leftmost, ffor, fanEither)
import qualified Reflex.Class as R
import Reflex.Dom.Core (HasValue(..))

import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Dom.Core (InputElement, EventResult)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PostBuild)


newtype RedeemLinkConfig t = RedeemLinkConfig
  { _redeemLinkConfig_errors :: Dynamic t (Maybe T.Text)
  }

-- | Parameterized on the DOM space 's' so consumers thread whatever
-- 'DomBuilderSpaceE es' resolves to in their effect row, instead of the
-- library hardcoding 'GhcjsDomSpace'. This lets the same record work
-- against static / hydration / immediate / prerender spaces.
data RedeemLinkData s t = RedeemLinkData
  { _redeemLink_email :: InputElement EventResult s t
  , _redeemLink_confirmEmail :: InputElement EventResult s t
  , _redeemLink_submit :: Event t ()
  }

-- | Widened error type: this family now accepts the full
-- 'BackendError RedeemLinkError' surface, so every backend branch
-- (BUserError, BCritical, BNoAuth family, BException, BInvalidRequest)
-- reaches the user via the 'ShowUser (BackendError e)' instance. This
-- eliminates the systemic 'fmapMaybe'-drop workarounds that used to
-- live in each consumer, which silently swallowed non-inner-error
-- branches.
-- | Effectful variant. See 'redeemLink_FRP' for the MTL twin.
redeemLink_FRPE
  :: forall req rsp s t es.
     ( PostBuild t :> es
     , Hold t :> es, Sample t :> es
     , RouteE.Routed t T.Text :> es
     , Reflex t

     )
  => ((T.Text, Email) -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError RedeemLinkError) T.Text)
  -> RedeemLinkData s t
  -> Eff es (RedeemLinkConfig t)
redeemLink_FRPE mkAPI sendRequest castResponse (RedeemLinkData email' eConfirm' submit) = do
  let email = value email'
  let eConfirm = value eConfirm'
  let
    checkMatch = (\(e,confirm) ->
                    if e == confirm
                    then Right e
                    else Left "Emails dont match"
                 )
    emails_agree = fmap checkMatch $ (,) <$> email <*> eConfirm
  let (bad, good) = fanEither (tag (current emails_agree) submit)
  codeLink <- RouteE.askRoute
  let req = ffor (attach (current codeLink) $ Email <$> good) mkAPI
  response <- sendRequest req
  let (err, res) = fanEither (fmap castResponse response) :: (Event t (BackendError RedeemLinkError), Event t T.Text)
  let errorsEv = Just <$> leftmost [ showUser <$> err
                                   , bad
                                   , "Success, please check your email" <$ res
                                   ]
  errors <- holdDyn Nothing errorsEv
  pure $ RedeemLinkConfig errors

-- | MTL twin of 'redeemLink_FRPE'.
redeemLink_FRP
  :: forall req rsp s t m.
     ( RouteM.Routed t T.Text m
     , R.MonadHold t m
     , Reflex t
     )
  => ((T.Text, Email) -> req)
  -> (Event t req -> m (Event t rsp))
  -> (rsp -> Either (BackendError RedeemLinkError) T.Text)
  -> RedeemLinkData s t
  -> m (RedeemLinkConfig t)
redeemLink_FRP mkAPI sendRequest castResponse (RedeemLinkData email' eConfirm' submit) = do
  let email = value email'
  let eConfirm = value eConfirm'
  let
    checkMatch = (\(e,confirm) ->
                    if e == confirm
                    then Right e
                    else Left "Emails dont match"
                 )
    emails_agree = fmap checkMatch $ (,) <$> email <*> eConfirm
  let (bad, good) = fanEither (tag (current emails_agree) submit)
  codeLink <- RouteM.askRoute
  let req = ffor (attach (current codeLink) $ Email <$> good) mkAPI
  response <- sendRequest req
  let (err, res) = fanEither (fmap castResponse response) :: (Event t (BackendError RedeemLinkError), Event t T.Text)
  let errorsEv = Just <$> leftmost [ showUser <$> err
                                   , bad
                                   , "Success, please check your email" <$ res
                                   ]
  errors <- R.holdDyn Nothing errorsEv
  pure $ RedeemLinkConfig errors
