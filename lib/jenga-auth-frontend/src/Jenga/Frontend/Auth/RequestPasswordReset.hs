{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RequestPasswordReset where

import Templates.Types
import Jenga.Common.Errors
import Jenga.Common.Auth

import Reflex (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)

import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)

newtype EmailForPasswordResetConfig t = EmailForPasswordResetConfig
  { _emailForPasswordResetConfig_errors :: Dynamic t (Maybe T.Text)
  }

data EmailForPasswordResetData t m = EmailForPasswordResetData
  { _emailForPasswordReset_email :: InputEl t m
  , _emailForPasswordReset_confirmEmail :: InputEl t m
  , _emailForPasswordReset_submit :: Event t ()
  }

requestPasswordReset_FRP
  :: forall req rsp t es.
     ( Dom t :> es
     , PostBuild t :> es
     , Hold t :> es
     , Reflex t
     , KnownTimeline es t
     )
  => (Email -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (EmailForPasswordResetData t (Eff es))
  -> Eff es (EmailForPasswordResetConfig t)
requestPasswordReset_FRP mkAPI sendRequest (EmailForPasswordResetData email' eConfirm' submit) = do
  let email = value email'
  let eConfirm = value eConfirm'
  let
    f = (\(a,b) ->
            if a == b
            then (Right a)
            else Left "Emails dont match"
        )
    emails_agree = fmap f $ (,) <$> email <*> eConfirm
  let (bad, good) = fanEither (tag (current emails_agree) submit)
  res <- sendRequest $ ffor good $ \goodEmail -> mkAPI $ Email goodEmail
  let (apiError, goodResponse) = fanEither (castResponse res) :: (Event t RequestPasswordResetError, Event t T.Text)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser <$> apiError
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv
  pure $ EmailForPasswordResetConfig errors
  where
    value = undefined -- TODO: InputEl accessor
    castResponse = undefined -- TODO: cast response type
