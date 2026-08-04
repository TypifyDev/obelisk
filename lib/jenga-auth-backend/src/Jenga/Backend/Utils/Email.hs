{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jenga.Backend.Utils.Email where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Schema

import Lamarckian.Types
import Lamarckian.Render
import qualified Reflex.Dom.Core as Rfx
import Rhyolite.Email
import Rhyolite.DB.NotifyListen.Beam
import Database.Beam.AutoMigrate.Compat
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema
import Database.Beam.Query
import Database.Beam.Backend.SQL
import Network.Mail.Mime

import Control.Exception as CE
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Aeson
import Data.Proxy
import Data.Pool
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

newtype AdminEmail = AdminEmail { getAdminEmail :: Address }


data MkEmail x = MkEmail
  { _mkEmail_subject :: T.Text
  , _mkEmail_body :: Rfx.StaticWidget x ()
  }

showPart :: Part -> String
showPart = \case { PartContent bs -> T.unpack . T.decodeUtf8 . LBS.toStrict $ bs ; _ -> ""} . partContent

-- | TODO(galen): dont use this everywhere but instead add to task list for Email runner
-- | also see sendEmailIfNotLocalOrUnsubscribed
sendEmailIfNotLocal :: MonadIO m => EmailConfig -> Mail -> m (Either T.Text ())
sendEmailIfNotLocal cfg mail = do
  route <- liftIO $ readFile "config/common/route"
  case isPrefixOf "http://localhost:" route || isInfixOf "ngrok-free.app" route of
    True -> do
      liftIO $ putStrLn "LOCALHOST EMAIL"
      liftIO . print . mailTo $ mail
      liftIO . putStrLn . take 1000  . mconcat . fmap showPart . mconcat . mailParts $ mail
      pure $ Right ()
    False -> do
      x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail
      liftIO $ print x
      case x of
        Left ioException -> pure $ Left (T.pack $ show ioException)
        Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
        Right (Right ()) -> pure $ Right ()

sendEmailIfNotLocalOrUnsubscribed
  :: forall db cfg es.
     ( IOE :> es
     , Database Postgres db
     , HasJengaTable Postgres db Unsubscribe
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg EmailConfig
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     )
  => Mail
  -> Eff es (Either T.Text ())
sendEmailIfNotLocalOrUnsubscribed mail = do
  cfg <- asksM @EmailConfig @cfg
  (unsubbed :: PgTable Postgres db Unsubscribe) <- asksTableM
  isLocalHostEnv @cfg >>= \case
    True -> do
      liftIO $ putStrLn "LOCALHOST EMAIL"
      liftIO $ print . mailTo $ mail
      liftIO $ print . take 1000 . mconcat . fmap showPart . mconcat . mailParts $ mail
      liftIO $ putStrLn "--------------------------"
      pure $ Right ()
    False -> do
      emails <- withDbEnv @cfg $ allUnsubscribedEmails unsubbed
      let mail' = mail { mailTo = filter (\mTo -> not $ addressEmail mTo `elem` emails)  $ mailTo mail }
      case null (mailTo mail') of
        True -> pure $ Right ()
        False -> do
          x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail'
          liftIO $ print x
          case x of
            Left ioException -> pure $ Left (T.pack $ show ioException)
            Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
            Right (Right ()) -> pure $ Right ()

allUnsubscribedEmails
  :: (Database Postgres db)
  => PgTable Postgres db Unsubscribe
  -> Pg [T.Text]
allUnsubscribedEmails tbl = fmap _unsubscribe_email <$> allUnsubscribed tbl

allUnsubscribed
  :: (Database Postgres db)
  => PgTable Postgres db Unsubscribe
  -> Pg [Unsubscribe Identity]
allUnsubscribed tbl = runSelectReturningList $ select $ all_ tbl

type HasJsonNotifyTbl be t n =
  ( BeamSqlBackend be
  , be ~ Postgres
  , Table t
  , FromBackendRow be (PrimaryKey t Identity)
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasNotification n t
  )

instance HasColumnType Mail where
  defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON Mail)

instance HasSqlValueSyntax PgValueSyntax Mail where
  sqlValueSyntax mail = sqlValueSyntax $ PgJSON mail


type EmailM cfg es db n be =
  ( Reader cfg :> es, HasConfig cfg AdminEmail
  , Reader cfg :> es, HasConfig cfg (Pool Connection)
  , IOE :> es
  , HasJengaTable Postgres db SendEmailTask
  , HasJsonNotifyTbl be SendEmailTask n
  )


newMkEmailHtml
  :: forall db cfg be es n x.
     ( EmailM cfg es db n be
     )
  => [Address]
  -> MkEmail x
  -> Eff es (Either T.Text ())
newMkEmailHtml toPlural mkEmail = do --subject widget = do
  newEmailHtml @db @cfg toPlural (_mkEmail_subject mkEmail) (_mkEmail_body mkEmail)

buildNewEmailHtml
  :: forall cfg es x.
     ( IOE :> es
     -- , EmailM es db n be
     , Reader cfg :> es, HasConfig cfg AdminEmail
     )
  => [Address]
  -> T.Text
  -> Rfx.StaticWidget x ()
  -> Eff es [Mail]
buildNewEmailHtml toPlural subject widget = do
  body <- liftIO $ fmap snd $ Rfx.renderStatic widget
  from <- getAdminEmail <$> asksM @AdminEmail @cfg -- toAddress <$> asksCfg _emailSendConfig
  forM toPlural $ \to -> do
    liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
  --pure $ Right ()


-- | TODO(galen) can we run Eff es in the context of StaticWidget?
newEmailHtml
  :: forall db cfg be es n x.
     ( EmailM cfg es db n be
     )
  => [Address]
  -> T.Text
  -> Rfx.StaticWidget x ()
  -> Eff es (Either T.Text ())
newEmailHtml toPlural subject widget = do
  body <- liftIO $ fmap snd $ Rfx.renderStatic widget
  from <- getAdminEmail <$> asksM @AdminEmail @cfg -- toAddress <$> asksCfg _emailSendConfig
  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  forM_ toPlural $ \to -> do
    mail <- liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
    fmap Right $ void $ withDbEnvQuiet @cfg $
      insertAndNotify emailTbl $ SendEmailTask
      { _sendEmailTask_id = default_
      , _sendEmailTask_finished = val_ False
      , _sendEmailTask_checkedOutBy = nothing_
      , _sendEmailTask_payload = val_ mail
      , _sendEmailTask_result = nothing_
      , _sendEmailTask_isUrgent = val_ (Just True)
      }
  pure $ Right ()

newHTemplateEmailHtml
  :: forall db cfg es k a r x be n.
     ( IOE :> es
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => [Address]
  -> T.Text
  -> HTemplateVars k a
  -> (HTemplateRefs k a -> StaticWidget' r x ())
  -> Eff es (Either T.Text ())
newHTemplateEmailHtml toPlural subject mappy mkDom = do
  --renderStaticTemplate' mappy $ widget
  body <- liftIO $ runStaticHTemplateWidget mappy mkDom
  --body <- liftIO $ fmap snd $ renderStatic widget
  from <- getAdminEmail <$> asksM @AdminEmail @cfg
  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  forM_ toPlural $ \to -> do
    mail <- liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
    pool <- asksM @(Pool Connection) @cfg
    fmap Right $ void $ runDb pool $
      insertAndNotify emailTbl $ SendEmailTask
      { _sendEmailTask_id = default_
      , _sendEmailTask_finished = val_ False
      , _sendEmailTask_checkedOutBy = nothing_
      , _sendEmailTask_payload = val_ mail
      , _sendEmailTask_result = nothing_
      , _sendEmailTask_isUrgent = val_ (Just True)
      }
  pure $ Right ()

newAdminEmail
  :: forall db cfg es be n.
     ( IOE :> es
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     --, Has (ComposeC ToJSON Identity) n
     )
  => T.Text
  -> T.Text
  -> Eff es ()
newAdminEmail subject body = do
  from <- getAdminEmail <$> asksM @AdminEmail @cfg
  newEmail @db @cfg @n from subject body

newEmail
  :: forall db cfg n es be.
     ( IOE :> es
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     --, Has (ComposeC ToJSON Identity) n
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Address
  -> T.Text
  -> T.Text
  -> Eff es ()
newEmail to subject body = do
  from <- getAdminEmail <$> asksM @AdminEmail @cfg -- toAddress <$> asksCfg _emailSendConfig
  let mail = simpleMail' to from subject . LT.fromStrict $ body
  -- Use produced mail to insert new SendEmailTask into the database
  -- Note that it does not reliably work to wakeup the worker within the
  -- transaction that emits the job, because it may not be visible to
  -- the worker until the transaction commits: This is a data race.
  --
  -- The solution is to rely on Postgres' NOTIFY semantics, which we handle
  -- in the notify handler.

  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  liftIO $ putStrLn "hey"
  void $ withDbEnv @cfg $
    insertAndNotify emailTbl $ SendEmailTask
    { _sendEmailTask_id = default_
    , _sendEmailTask_finished = val_ False
    , _sendEmailTask_checkedOutBy = nothing_
    , _sendEmailTask_payload = val_ mail
    , _sendEmailTask_result = nothing_
    , _sendEmailTask_isUrgent = val_ (Just True)
    }
