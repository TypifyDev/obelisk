module Jenga.Backend.Handlers.Auth.Subscriptions.NewSubscriber where

import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Stripe
import Jenga.Common.Schema

import Rhyolite.Account as Rhy
import Rhyolite.Backend.Account as RhyB
import Database.Beam.Schema
import Database.Beam.Postgres
import Jenga.Route
import qualified Reflex.Dom.Core as Rfx
import Data.Signed

import Network.Mail.Mime
import Web.Stripe as S
import Web.Stripe.Customer as S
import Web.Stripe.Subscription as S
import Web.Stripe.Token as S
import Web.Stripe.Plan as S
import Web.ClientSession as CS

import Control.Monad.IO.Class
import Data.Pool
import Data.Aeson (FromJSON)
import Data.Time.Clock (nominalDay, addUTCTime, getCurrentTime)

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

-- Change to Confirm Subscription
newSubscriberHandler
  :: forall api db cfg be frontendRoute es n.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg StripeConfig
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg Plans
     , Reader cfg :> es, HasConfig cfg CS.Key
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , HasJengaTable Postgres db Rhy.Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db FreeTrial
     , HasJengaTable Postgres db StripeRelation
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Proxy api
  -> Id Rhy.Account
  -> PaymentFormPrivate
  -> frontendRoute (Signed PasswordResetToken)
  -> Eff es (Either (BackendError SubscribeError) Bool)
newSubscriberHandler proxy acctID form resetRoute = do
  stripeConfig <- asksM @StripeConfig @cfg --  _stripeConfig
  (acctTbl :: PgTable Postgres db Rhy.Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (stripeTbl :: PgTable Postgres db StripeRelation) <- asksTableM
  (orgTbl :: PgTable Postgres db OrganizationEmails) <- asksTableM

  email'_code <- withDbEnv @cfg $ do
    e <- getUsersEmail acctTbl acctID
    dc <- getUsersDiscountCode freeTrialTbl acctID
    pure $ (,) <$> e <*> dc
  case email'_code of
    Nothing -> pure $ Left . BCritical $ MissingEmail --"email not found"
    Just (email, discountCode) -> do
      let
        cardNum = _paymentForm_cardNumber form
        expMonth = _paymentForm_expiryMonth form
        expYear = _paymentForm_expiryYear form
        cvc = _paymentForm_cvc form
        userCard = (mkNewCard (CardNumber cardNum) (ExpMonth expMonth) (ExpYear expYear)) { newCardCVC = Just (CVC cvc) }
        stripe' :: (MonadIO m2, FromJSON (StripeReturn a)) => StripeRequest a -> m2 (Either StripeError (StripeReturn a))
        stripe' = liftIO . stripe stripeConfig
      liftIO $ print discountCode
      liftIO $ print userCard
      eithCardToken <- liftIO $ stripe' $ createCardToken (Just userCard)
      case eithCardToken of
        Left e -> pure . Left . BCritical . StripeGenError $ e
        Right token -> do
          liftIO $ print token
          eithCustomer :: Either StripeError Customer <- stripe' $ createCustomer
                                                         -&- (S.Email email)
                                                         -&- (tokenId token)
          case eithCustomer of
            Left stripeError -> pure . Left . BCritical . StripeGenError $ stripeError
            Right DeletedCustomer{} -> pure . Left . BCritical $ StripeCustomerDeleted
            Right Customer{customerId = cid} -> do
              liftIO $ print cid
              --StripeCode validCode <- lookupSubscriptionCodeEnv discountCode
              mValidCode <- lookupSubscriptionCodeEnv @cfg discountCode
              case mValidCode of
                Nothing -> pure . Left . BUserError $ NoMatchingPlanFound
                Just (StripePlan validCode)  -> do
                  planResult <- stripe' . getPlan $ PlanId validCode
                  case planResult of
                    Left stripeError2 -> pure . Left . BCritical . StripeGenError $ stripeError2
                    Right plan -> do
                      liftIO $ print plan
                      sevenDaysFromNow <- liftIO $ ((nominalDay * 7) `addUTCTime`) <$> getCurrentTime
                      let trialEnd = TrialEnd sevenDaysFromNow
                      (isNew, aid) <- withDbEnv @cfg $ ensureAccountExists' acctTbl email
                      -- let (AccountId (SqlSerial rawID)) = aid
                      --reportLog False $ NewSubscription (fromIntegral rawID) email
                      case isNew of
                        False -> do
                          mInfo <- withDbEnv @cfg $ getStripeInfo stripeTbl aid
                          case mInfo of
                            Nothing -> do
                              withDbEnv @cfg $ isOrgEmail orgTbl email >>= \case
                                False -> pure $ Left . BCritical $ NoAssociatedStripeInfo
                                True -> pure $ Left . BUserError $ AlreadyInGroupSubscription
                            Just (StripeRelation _ _ subId) ->
                              case subId of
                                Just _ -> pure $ Left . BUserError $ AlreadyInSelfSubscription
                                Nothing -> do
                                  -- Remove trial end, we handle this ourselves
                                  eithSubscription <- stripe' $ createSubscription cid (planId plan) -&- trialEnd
                                  case eithSubscription of
                                    Left subscriptionError -> do
                                      pure . Left . BCritical . StripeGenError $ subscriptionError
                                    Right sub -> do
                                      withDbEnv @cfg $ do
                                        recordResubscribe stripeTbl aid cid (subscriptionId sub)
                                      pure $ Right False
                        True -> do
                          mNonce <- withDbEnv @cfg $ do
                            putNewUserType uTypeTbl aid Nothing
                            newNonce acctTbl aid
                          case mNonce of
                            Nothing -> pure $ Left . BCritical $ Stripe_FailedMkNonce
                            Just noncense -> do
                              csk <- asksM @CS.Key @cfg -- Cfg _clientSessionKey
                              authToken <- withDbEnv @cfg $ passwordResetToken csk aid noncense
                              eithSubscription <- stripe' $ createSubscription cid (planId plan) -&- trialEnd
                              case eithSubscription of
                                Left subscriptionError -> do
                                  withDbEnv @cfg $ deleteFailedAccount acctTbl email
                                  pure . Left . BCritical . StripeGenError $ subscriptionError
                                Right sub -> do
                                  withDbEnv @cfg $ putNewStripeInfo stripeTbl aid cid (subscriptionId sub)
                                  link <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy $ resetRoute :/ authToken
                                  let
                                    to = Address
                                      { addressName = Nothing
                                      , addressEmail = email
                                      }
                                  sendEmail <- newEmailHtml @db @cfg [to] "Subscription Confirmation" $ do
                                    Rfx.el "div" $ do
                                      Rfx.el "div" $ Rfx.text "New account creation requested"
                                      Rfx.el "div" $ Rfx.text "go the following address to set your password:"
                                      Rfx.el "div" $ Rfx.text $ getLink link
                                      Rfx.el "div" $ Rfx.text ""
                                      Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."
                                  case sendEmail of
                                    Left _ -> pure . Left . BCritical $ SubFailedEmail email
                                    Right _ -> pure $ Right True
