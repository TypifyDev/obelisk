-- TODO: move to Stripe handler folder

module Jenga.Backend.Handlers.Auth.Subscriptions.CancelSubscription where


import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased (isOrgOwnedUser)
import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.BeamExtras
import Jenga.Common.Auth

import Database.Beam
import Database.Beam.Postgres
import Rhyolite.Account

import Data.Pool
import Control.Monad.IO.Class ()

import Web.Stripe
import Web.Stripe.Customer
import Web.Stripe.Subscription

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as ER

cancelSubscriptionHandler
  :: forall db cfg es.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg StripeConfig
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db StripeRelation
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db OrgOwnedUsers
     )
  => Id Account
  -> Eff es (Either (BackendError CancelSubError) ())
cancelSubscriptionHandler acctID = do
  stripeConfig <- ER.asks (fromCfg :: cfg -> StripeConfig)

  (stripeTbl :: PgTable Postgres db StripeRelation) <- asksTableM
  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  (orgTbl :: PgTable Postgres db OrgOwnedUsers) <- asksTableM

  let stripe' = liftIO . stripe stripeConfig
  maybeStripeRelation <- withDbEnvE @cfg $ getStripeInfo stripeTbl acctID
  case maybeStripeRelation of
    Nothing -> do
      mEmail <- withDbEnvE @cfg $ getUsersEmail acctTbl acctID
      case mEmail of
        Nothing -> pure $ Left . BCritical $ NoSubOrAccount
        Just _email -> do
          -- If the user has no Stripe subscription but their account is in the
          -- OrgOwnedUsers table, they were paid for as part of a bulk /
          -- group purchase. Cancelling on their own is a user error; the org
          -- admin must handle it.
          isBulkPurchaseEmail <- withDbEnvE @cfg $ isOrgOwnedUser orgTbl acctID
          case isBulkPurchaseEmail of
            True -> pure $ Left . BUserError $ SelfCancelUnderGroupPlan
            False -> pure $ Left . BCritical $ NoStripeInfo
    Just (StripeRelation _ customerId_ maybeSubscriptionId) -> do
      case maybeSubscriptionId of
        Nothing -> pure $ Left . BUserError $ NoSubscriptionExists
        Just subscriptionId_ -> do
          eithCancelSub <- stripe' $ cancelSubscription (CustomerId customerId_) (SubscriptionId subscriptionId_)
          case eithCancelSub of
            Left e -> pure . Left . BCritical . StripeCouldntCancel $ e
            Right _ -> do
              withDbEnvE @cfg $ recordEndSubscription stripeTbl acctID
              pure $ Right ()

  -- get stripe customer id AND subscription id based on acct id
  -- use stripe api to cancel subscription for this customer

  -- the subscription ID should be a Maybe subscription ID where Nothing implies the subscription was cancelled
  -- then if they re-subscribe it will be a new Just subscriptionId
