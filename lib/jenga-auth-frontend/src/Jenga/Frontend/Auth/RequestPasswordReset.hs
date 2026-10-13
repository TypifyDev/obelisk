{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RequestPasswordReset where
import Reflex.Host.HeadlessEffectful (NotReady)
import Reflex.Host.HeadlessEffectful (Adjustable)

import Jenga.Common.Errors
import Jenga.Common.Auth

import Reflex.Class (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)
import qualified Reflex.Class as R
import Reflex.Dom.Core (HasValue(..))

import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Dom.Core (InputElement, EventResult, DomBuilderSpace)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Effectful.DomHost (DomBuilderE, DomBuilderSpaceE)
import Reflex.Host.HeadlessEffectful (PostBuild)

newtype EmailForPasswordResetConfig t = EmailForPasswordResetConfig
  { _emailForPasswordResetConfig_errors :: Dynamic t (Maybe T.Text)
  }

-- | Space-parameterized: 's' is the DomBuilderE space
-- (previously pinned to GhcjsDomSpace). Consumers pass whatever space
-- their surrounding widget uses; callers in 'requestPasswordReset_FRP'
-- receive 'DomBuilderSpaceE es'.
data EmailForPasswordResetData s t m = EmailForPasswordResetData
  { _emailForPasswordReset_email :: InputElement EventResult s t
  , _emailForPasswordReset_confirmEmail :: InputElement EventResult s t
  , _emailForPasswordReset_submit :: Event t ()
  }

-- | MTL twin alias: pins @s@ to the ambient @DomBuilderSpace m@ so MTL
-- callers write 'EmailForPasswordResetDataMTL t m' instead of
-- 'EmailForPasswordResetData (DomBuilderSpace m) t m'.
type EmailForPasswordResetDataMTL t m =
  EmailForPasswordResetData (DomBuilderSpace m) t m

-- | Widened error type: this family now accepts the full
-- 'BackendError RequestPasswordResetError' surface, so every backend
-- branch (BUserError, BCritical, NoAuth family, BException,
-- BInvalidRequest) reaches the user via the 'ShowUser (BackendError e)'
-- instance. This eliminates the systemic 'fmapMaybe'-drop workarounds
-- that used to live in each consumer, which silently swallowed
-- non-inner-error branches.
-- | Effectful variant. See 'requestPasswordReset_FRP' for the MTL twin.
requestPasswordReset_FRPE
  :: forall req rsp t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , PostBuild t :> es
     , Hold t :> es, Sample t :> es
     , Reflex t

     )
  => (Email -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError RequestPasswordResetError) T.Text)
  -> (EmailForPasswordResetData (DomBuilderSpaceE es) t (Eff es))
  -> Eff es (EmailForPasswordResetConfig t)
requestPasswordReset_FRPE mkAPI sendRequest castResponse (EmailForPasswordResetData email' eConfirm' submit) = do
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
  let (apiError, goodResponse) = fanEither (fmap castResponse res) :: (Event t (BackendError RequestPasswordResetError), Event t T.Text)
  -- 'showUser' on 'BackendError e' renders every branch; nothing is
  -- dropped.
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser <$> apiError
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv
  pure $ EmailForPasswordResetConfig errors

-- | MTL twin of 'requestPasswordReset_FRPE'. Pure FRP wiring: reads
-- 'value' from the two 'InputElement's (via 'HasValue'), validates the
-- confirm-email match, fires the caller-supplied 'sendRequest' (which
-- itself runs in @m@), and returns an 'EmailForPasswordResetConfig'.
-- No DOM is constructed here, so no 'DomBuilder' / 'Prerender'
-- constraints — any Reflex MTL monad with 'MonadHold' works.
requestPasswordReset_FRP
  :: forall req rsp t m.
     ( R.MonadHold t m
     , Reflex t
     )
  => (Email -> req)
  -> (Event t req -> m (Event t rsp))
  -> (rsp -> Either (BackendError RequestPasswordResetError) T.Text)
  -> EmailForPasswordResetDataMTL t m
  -> m (EmailForPasswordResetConfig t)
requestPasswordReset_FRP mkAPI sendRequest castResponse (EmailForPasswordResetData email' eConfirm' submit) = do
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
  let (apiError, goodResponse) = fanEither (fmap castResponse res) :: (Event t (BackendError RequestPasswordResetError), Event t T.Text)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser <$> apiError
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- R.holdDyn Nothing errorsEv
  pure $ EmailForPasswordResetConfig errors
