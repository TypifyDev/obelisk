module Jenga.Backend.Utils.Log where

import Jenga.Common.Log
import Jenga.Common.Schema
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable

import Rhyolite.DB.Beam
import Database.Beam.Query
import Database.Beam.Postgres

import Control.Monad.IO.Class ()
import Control.Exception
import Data.Pool
import qualified Data.Text as T
import Prelude hiding (log)

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


reportError
  :: forall db cfg es be n a.
    ( IOE :> es
    , Show a
    , HasJengaTable Postgres db SendEmailTask
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => a
  -> Eff es ()
reportError = newAdminEmail @db @cfg @es @be @n "Error on production" . T.pack . show

reportOnError
  :: forall db cfg es be n a b.
    ( IOE :> es
    , Show a
    , HasJengaTable Postgres db SendEmailTask
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => Either a b -> Eff es ()
reportOnError = \case
  Left err -> reportError @db @cfg @es @be @n err
  Right _ -> pure ()

catchToss :: IO a -> IO ()
catchToss m = catch (m >> pure ()) (\(_ :: IOException) -> pure ())

reportLog
  :: forall db cfg es log.
     ( Loggable log
     , IOE :> es
     , HasJengaTable Postgres db LogItemRow
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     )
  => Bool
  -> log
  -> Eff es ()
reportLog isUrgent item = do
  (logTbl :: PgTable Postgres db LogItemRow) <- asksTableM
  withDbEnv @cfg $ do
    runInsert $ insert logTbl $ insertExpressions
      [ LogItemRow
        default_
        (val_ False)
        current_timestamp_
        (val_ isUrgent)
        (val_ $ renderLog item)
      ]
