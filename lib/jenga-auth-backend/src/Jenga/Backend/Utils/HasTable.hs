module Jenga.Backend.Utils.HasTable where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Database.Beam.Postgres
import Database.Beam.Schema
import Data.Pool

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


class HasJengaTable dbHost db tbl where
  tableRef :: PgTable dbHost db tbl

type PgTable dbHost db x = DatabaseEntity dbHost db (TableEntity x)

asksTableM
  :: (HasJengaTable dbHost db tbl)
  => Eff es (PgTable dbHost db tbl)
asksTableM = do
  pure tableRef

withDbEnv :: forall cfg es a. (Reader cfg :> es, HasConfig cfg (Pool Connection), IOE :> es) => Pg a -> Eff es a
withDbEnv query_ = flip runSerializable query_ =<< asksM @(Pool Connection) @cfg

withDbEnvQuiet :: forall cfg es a. (Reader cfg :> es, HasConfig cfg (Pool Connection), IOE :> es) => Pg a -> Eff es a
withDbEnvQuiet query = do
  dbConn <- asksM @(Pool Connection) @cfg
  runDbQuiet dbConn query
