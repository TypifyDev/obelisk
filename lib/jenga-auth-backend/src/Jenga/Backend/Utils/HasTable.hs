module Jenga.Backend.Utils.HasTable where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Database.Beam.Postgres
import Database.Beam.Schema
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class
import Data.Pool

import Effectful (Eff, (:>), IOE)
import qualified Effectful.Reader.Static as ER
import qualified Data.Text as T
import qualified Jenga.Route as ObR
import Jenga.Route (renderObeliskFrontendRoute, renderObeliskBackendRoute)


class HasJengaTable dbHost db tbl where
  tableRef :: PgTable dbHost db tbl

type PgTable dbHost db x = DatabaseEntity dbHost db (TableEntity x)

asksTableM
  :: (HasJengaTable dbHost db tbl, Applicative m)
  => m (PgTable dbHost db tbl)
asksTableM = pure tableRef

withDbEnv
  :: forall cfg m a. (HasConfig cfg (Pool Connection), MonadIO m, MonadReader cfg m)
  => Pg a -> m a
withDbEnv query_ = flip runSerializable query_ =<< asksM @(Pool Connection)

withDbEnvQuiet
  :: forall cfg m a. (HasConfig cfg (Pool Connection), MonadIO m, MonadReader cfg m)
  => Pg a -> m a
withDbEnvQuiet query = do
  dbConn <- asksM @(Pool Connection)
  runDbQuiet dbConn query

withDbEnvE
  :: forall cfg es a.
     ( HasConfig cfg (Pool Connection)
     , IOE :> es
     , ER.Reader cfg :> es
     )
  => Pg a -> Eff es a
withDbEnvE query_ = do
  dbConn <- ER.asks (fromCfg :: cfg -> Pool Connection)
  liftIO $ runSerializable dbConn query_

withDbEnvQuietE
  :: forall cfg es a.
     ( HasConfig cfg (Pool Connection)
     , IOE :> es
     , ER.Reader cfg :> es
     )
  => Pg a -> Eff es a
withDbEnvQuietE query_ = do
  dbConn <- ER.asks (fromCfg :: cfg -> Pool Connection)
  liftIO $ runDbQuiet dbConn query_

renderFullRouteFEE
  :: forall be fe cfg es.
     ( HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , ER.Reader cfg :> es
     )
  => ObR.R fe -> Eff es Link
renderFullRouteFEE route = do
  enc     <- ER.asks (fromCfg :: cfg -> FullRouteEncoder be fe)
  baseUrl <- ER.asks (fromCfg :: cfg -> BaseURL)
  pure . Link $ (T.pack . show . getBaseURL $ baseUrl) <> renderObeliskFrontendRoute enc route

renderFullRouteBEE
  :: forall fe be cfg es.
     ( HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , ER.Reader cfg :> es
     )
  => ObR.R be -> Eff es Link
renderFullRouteBEE route = do
  enc     <- ER.asks (fromCfg :: cfg -> FullRouteEncoder be fe)
  baseUrl <- ER.asks (fromCfg :: cfg -> BaseURL)
  pure . Link $ (T.pack . show . getBaseURL $ baseUrl) <> renderObeliskBackendRoute enc route
