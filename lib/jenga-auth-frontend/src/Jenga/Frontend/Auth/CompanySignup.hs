{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Jenga.Frontend.Auth.CompanySignup where

import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Templates.Types
import Jenga.Common.Errors

import Jenga.Route.Frontend (Routed, askRoute)
import Reflex (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)

import Control.Monad (join)
import Data.Proxy (Proxy)
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.Prerender (Prerender)


data SignupConfig t = SignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }


data SignupData t m = SignupData
  { _signup_email :: InputEl t m
  , _signup_confirmEmail :: InputEl t m
  , _signup_orgName :: InputEl t m
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }


-- | TODO(anyone): It would be really cool to populate forms with info they've already entered so that they can move forward faste
-- | we could do this through query params
newCompanySignup_FRP
  :: forall req rsp t es.
     ( Dom t :> es
     , Hold t :> es
     , PostBuild t :> es
     , Prerender t :> es
     , Routed t (Map.Map T.Text (Maybe T.Text)) :> es
     , Reflex t
     , KnownTimeline es t
     )
  => (NewCompanyEmail -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (SignupData t (Eff es))
  -> Eff es (SignupConfig t)
newCompanySignup_FRP mkAPI sendRequest (SignupData email' eConfirm' orgName' agree' submit) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let orgName = value orgName'
  let
    validate = (\(emailA,emailB,orgName_, agree'', code') ->
           if agree'' == False
           then Left "You must accept terms"
           else
             if emailA /= emailB
             then Left "Emails dont match"
             else
               case code' of
                 Nothing -> Left "No organization code found, please get link from lauren@aceinterviewprep.io"
                 Just code'' ->
                   case EmailValidate.validate $ T.encodeUtf8 emailA of
                     Left _ -> Left "Invalid Email"
                     Right e ->
                       Right $ NewCompanyEmail e orgName_ code''
               )
    valid = fmap validate $ (,,,,) <$> email <*> eConfirm <*> orgName <*> agree <*> code
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' mkAPI
  response <- sendRequest request
  let (errRes, goodResponse) = fanEither (castResponse response) :: (Event t (BackendError UserSignupError), Event t T.Text)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv

  pure $ SignupConfig errors (() <$ goodResponse)
  where
    value = undefined -- TODO: InputEl accessor
    castResponse = undefined -- TODO: cast response type

-- TODO: restore terms display. Same t-ambiguity as in UserSignup.
seeTerms :: forall t es. (Dom t :> es, Reflex t) => Proxy t -> Eff es ()
seeTerms _proxy = pure ()
