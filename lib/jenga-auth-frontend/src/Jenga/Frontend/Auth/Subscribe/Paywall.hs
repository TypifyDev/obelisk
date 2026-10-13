{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.Subscribe.Paywall where

import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Common.Stripe
import Jenga.Route.Frontend (SetRoute, setRoute)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither, ffilter)

import qualified Data.Text as T
import Text.Read

import Effectful (Eff, (:>), IOE)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PostBuild, getPostBuildE)
import Reflex.Host.HeadlessEffectful (PerformEventH, PerformEventM, PerformableForRow)
import qualified Reflex.Host.Class as RH
import Control.Monad.IO.Class (MonadIO)
import Reflex.Host.HeadlessEffectful (TriggerEvent)
import Reflex.Time (delay)


data PaywallData t = PaywallData
  { _paywallData_firstName :: Dynamic t T.Text
  , _paywallData_lastName  :: Dynamic t T.Text
  , _paywallData_cardNumber :: Dynamic t T.Text
  , _paywallData_expiryMonth :: Dynamic t T.Text
  , _paywallData_expiryYear :: Dynamic t T.Text
  , _paywallData_cvc :: Dynamic t T.Text
  , _paywallData_click :: Event t ()
  }

-- code userMessage errorEv
data PaywallConfig t = PaywallConfig
  { _paywallConfig_code :: Dynamic t (Maybe T.Text)
  , _paywallConfig_userMessage :: Dynamic t T.Text
  , _paywallConfig_errors :: Event t FrontendError
  }

paywall_FRP
  :: forall frontendR t es.
     ( PostBuild t :> es
     , Hold t :> es, Sample t :> es
     , PerformEventH t :> es
     , PerformEventM (RH.HostFrame t) t :> es
     , PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , TriggerEvent t :> es
     , SetRoute t frontendR :> es
     , IOE :> es
     , Reflex t

     )
  => frontendR
  -> (Event t () -> Eff es (Event t (Either (BackendError NoFreeTrialCode) (T.Text, Maybe T.Text))))
  -> (Event t PaymentFormPrivate -> Eff es (Event t (Either (BackendError SubscribeError) Bool)))
  -> PaywallData t
  -> Eff es (PaywallConfig t)
paywall_FRP loginRoute sendHasFreeTrialCode sendUpgradeToPaid (PaywallData _firstName _lastName cardNumber expiryMonth expiryYear cvc click) = do
  pb <- getPostBuildE
  codeResponse <- sendHasFreeTrialCode pb
  let (errFT, email_code :: Event t (T.Text, Maybe T.Text)) = fanEither codeResponse

  let code' = snd <$> email_code
  code <- holdDyn Nothing code'
  let (validEmailAndNumericals) =
        tag (current $ (,,,,)
             <$> ((readEither . T.unpack :: T.Text -> Either String Int) <$> expiryMonth)
             <*> ((readEither . T.unpack :: T.Text -> Either String Int) <$> expiryYear)
             <*> cardNumber
             <*> cvc
             <*> code
            ) click
  let
    validate :: ( Either String Int
                , Either String Int
                , T.Text
                , T.Text
                , Maybe T.Text
                ) -> Either String PaymentFormPrivate
    validate (expiryMonth', expiryYear', cardNumber', cvc', discountCode') =
      case expiryMonth' of
        Left _ -> Left "Invalid Expiry Month"
        Right month -> case expiryYear' of
          Left _ -> Left "Invalid Expiry Year"
          Right year -> Right $ PaymentFormPrivate cardNumber' month year cvc' discountCode'

  -- we should (maybe) sign this for security like the tokens (may be nicer to do as fields of payment form)
  -- it may also be redundant since the key would need to be shared (i think)
  let (err, paymentFormGood) = fanEither $ validate <$> validEmailAndNumericals
  upgradeResponse <- sendUpgradeToPaid paymentFormGood
  let (errRes, good) = fanEither upgradeResponse

  let
    onGood = ffor good $ \case
      True -> "Thank you for subscribing to Ace! Check your email to complete onboarding now and get ready to start interviewing.";
      False -> "Success, Account Re-activated, redirecting now.."
    paymentFormGood' = "Hang tight, we are confirming your subscription..." <$ paymentFormGood

  let
    errorEv = leftmost
      [ Req . Request_ErrorAPI <$> errRes
      , UserError . T.pack <$> err
      , Req . Request_ErrorAPI <$> errFT
      ]
  userMessage <- holdDyn "" $ leftmost [ showUser <$> errorEv, paymentFormGood', onGood ]
  redirect <- delay 2 (ffilter (== False) good)
  setRoute $ loginRoute <$ (ffilter (== False) redirect)

  pure $ PaywallConfig code userMessage errorEv
