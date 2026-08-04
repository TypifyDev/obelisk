{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jenga.Frontend.Auth.UserSignup where

import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Templates.Types
import Jenga.Common.Errors
import Jenga.Route.Frontend ()
import Reflex (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)
import Reflex.Dom.Core (HasValue(..))

import Control.Monad ()
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Data.Proxy (Proxy)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.Prerender (Prerender)

data UserSignupConfig t = UserSignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }

data UserSignupData t m = UserSignupData
  { _signup_email :: InputEl t m
  , _signup_confirmEmail :: InputEl t m
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }

newUserSignup_FRP
  :: forall req rsp t es.
     ( Dom t :> es
     , Hold t :> es
     , PostBuild t :> es
     , Prerender t :> es
     , Reflex t
     , KnownTimeline es t
     )
  => (EmailValidate.EmailAddress -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError UserSignupError) T.Text)
  -> UserSignupData t (Eff es)
  -> Eff es (UserSignupConfig t)
newUserSignup_FRP mkAPI sendRequest castResponse (UserSignupData email' eConfirm' agree' submit) = do
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let
    validate (emailA, emailB, agree'') =
      if not agree''
      then Left "You must accept terms"
      else if emailA /= emailB
      then Left "Emails dont match"
      else case EmailValidate.validate $ T.encodeUtf8 emailA of
        Left _ -> Left "Invalid Email"
        Right e -> Right e
    valid = fmap validate $ (,,) <$> email <*> eConfirm <*> agree
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' mkAPI
  response <- sendRequest request
  let (errRes, goodResponse) = fanEither (fmap castResponse response) :: (Event t (BackendError UserSignupError), Event t T.Text)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv
  pure $ UserSignupConfig errors (() <$ goodResponse)

-- TODO: restore terms display. Original body was ambiguous in `t` because
-- elClass's KnownTimeline fundep couldn't unify without a caller-side pin.
seeTerms :: forall t es. (Dom t :> es, Reflex t) => Proxy t -> Eff es ()
seeTerms _proxy = pure ()
