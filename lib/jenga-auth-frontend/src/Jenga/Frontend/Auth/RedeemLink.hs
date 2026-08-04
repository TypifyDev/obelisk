{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RedeemLink where

import Templates.Types
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Route.Frontend (Routed, askRoute)
import Reflex (Reflex, Event, Dynamic, current, tag, attach, leftmost, ffor, fanEither)

import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)


newtype RedeemLinkConfig t = RedeemLinkConfig
  { _redeemLinkConfig_errors :: Dynamic t (Maybe T.Text)
  }

data RedeemLinkData t m = RedeemLinkData
  { _redeemLink_email :: InputEl t m
  , _redeemLink_confirmEmail :: InputEl t m
  , _redeemLink_submit :: Event t ()
  }

redeemLink_FRP
  :: forall req rsp t es.
     ( Dom t :> es
     , PostBuild t :> es
     , Hold t :> es
     , Routed t T.Text :> es
     , Reflex t
     , KnownTimeline es t
     )
  => ((T.Text, Email) -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> RedeemLinkData t (Eff es)
  -> Eff es (RedeemLinkConfig t)
redeemLink_FRP mkAPI sendRequest (RedeemLinkData email' eConfirm' submit) = do
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
  codeLink <- askRoute
  let req = ffor (attach (current codeLink) $ Email <$> good) mkAPI
  response <- sendRequest req
  let (err, res) = fanEither (castResponse response) :: (Event t RedeemLinkError, Event t T.Text)

  let errorsEv = Just <$> leftmost [ showUser <$> err
                                   , bad
                                   , "Success, please check your email" <$ res
                                   ]
  errors <- holdDyn Nothing errorsEv
  pure $ RedeemLinkConfig errors
  where
    value = undefined -- TODO: InputEl accessor
    castResponse = undefined -- TODO: cast response type
