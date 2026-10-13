{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Ported from upstream @Reflex.Dom.WebSocket.Query@. The two user-facing
-- functions (@cropQueryT@ / @runWebSocketQuery@) are the WebSocket adapter
-- for the 'Query' effect: they turn an @Eff@ scope that submits queries
-- via 'tellQueryIncremental' \/ 'queryIncremental' into a running
-- JSON-encoded WebSocket loop that exchanges request/response with a
-- server.
--
-- The MTL wrapper was timeline-polymorphic (@Reflex t@). This port pins
-- @t ~ EffTimeline Core.Global@ because 'runQuery' — the underlying
-- 'Query' effect interpreter in @reflex-effectful@ — produces a Core
-- 'PrimInc.Incremental Core.Global (AdditivePatch q)' and consumes a
-- Core 'CoreT.Dynamic Core.Global (QueryResult q)'. Bridging those to
-- the class-level 'R.Incremental' / 'R.Dynamic' / 'R.Event' families is
-- done via the newtype wrappers 'EffIncremental' / 'EffDynamic' /
-- 'EffEvent' that @Reflex.Class@ exposes for exactly
-- this kind of interop.
module Reflex.Effectful.Dom.WebSocket.Query
  ( cropQueryT
  , runWebSocketQuery
  ) where

import           Data.Aeson                                    (FromJSON, ToJSON)
import           Data.Default                                  (def)
import           Data.Maybe                                    (fromMaybe)
import           Data.Patch                                    (AdditivePatch, Group)
import           Data.Semigroup.Commutative                    (Commutative)
import           Data.Text                                     (Text)
import           Effectful                                     (Eff, IOE, (:>))

import           Reflex.Class                        (Dynamic, Event, Push, attach, current, foldDyn, incrementalToDynamic, leftmost, tag, updated)
import qualified Reflex.Class                        as R
import           Reflex.Query.Class            (Query, QueryResult, crop)
import           Reflex.Class                   (Dynamic (unEffDynamic), Incremental (EffIncremental))
import           Reflex.Class                  (Hold)
import           Reflex.Class                (Sample)
import qualified Reflex.Class                as Core
import qualified Reflex.Class                   as CoreT
import           Reflex.Class                      (holdUniqDyn)
import           Reflex.Class          (PerformEvent)
import           Reflex.Class             (PostBuild, getPostBuild)
import qualified Reflex.Class                 as QueryEff
import           Reflex.Class          (TriggerEvent)
import           Reflex.Dom.Core  (JSM')
import           Reflex.Dom.Core                (RawWebSocket (..), WebSocketConfig (..), jsonWebSocket)

-- | Local alias: the one timeline this module operates on. See the
-- module-level Haddock for the reason the MTL polymorphism collapses
-- here.
type T = Core.EffTimeline Core.Global

-- | Run a 'Query' scope talking to a JSON WebSocket server. Direct port
-- of @Reflex.Dom.WebSocket.Query.runWebSocketQuery@.
--
-- The recursive @rec@ binds mirror the MTL original: the scope's
-- outgoing 'Incremental' becomes the send-payload stream (fed on both
-- @updated request@ and @tag (current request) postBuild@), and the
-- server's decoded responses feed back in as the ambient 'QueryResult'.
runWebSocketQuery
  :: forall es q a.
     ( ToJSON q
     , FromJSON (QueryResult q)
     , Query q
     , Commutative q
     , Group q
     , Eq q
     , IOE :> es
     , JSM' :> es
     
     , PostBuild T :> es
     , TriggerEvent T :> es
     , PerformEvent T :> es
     , Hold T :> es
     , Sample T :> es
     , Push T :> es
     )
  => Eff (QueryEff.Query T q : es) a
  -> Text -- ^ WebSocket url
  -> Eff es a
runWebSocketQuery app url = do
  postBuild <- getPostBuild @_ @T
  rec ws <- jsonWebSocket url $ def { _webSocketConfig_send = pure <$> updatedRequest }
      (a, request) <- cropQueryT app $ fromMaybe mempty <$> _webSocket_recv ws
      let updatedRequest = leftmost [updated request, tag (current request) postBuild]
  pure a

-- | Direct port of @Reflex.Dom.WebSocket.Query.cropQueryT@. Runs a
-- 'Query'-effect scope against a stream of decoded 'QueryResult's,
-- returning the scope's value plus the aggregate outgoing 'Query'.
--
-- The scope's aggregate 'Incremental' (as produced by 'runQuery') is
-- deduplicated with 'holdUniqDyn' and used both to feed the outgoing
-- request stream and to 'crop' the incoming 'QueryResult' stream down
-- to the piece relevant to the current 'Query'.
cropQueryT
  :: forall es q a.
     ( Query q
     , Commutative q
     , Group q
     , Eq q
     
     , Hold T :> es
     , Sample T :> es
     , Push T :> es
     )
  => Eff (QueryEff.Query T q : es) a
  -> Event T (QueryResult q)
  -> Eff es (a, Dynamic T q)
cropQueryT app result = do
  rec (a, requestPatch) <- QueryEff.runQuery @T ambientResult app
      let requestInc :: R.Incremental T (AdditivePatch q)
          requestInc = EffIncremental requestPatch
      requestUniq <- holdUniqDyn (incrementalToDynamic requestInc)
      croppedResult <- cropDyn requestUniq result
      let ambientResult :: CoreT.Dynamic Core.Global (QueryResult q)
          ambientResult = unEffDynamic croppedResult
  pure (a, requestUniq)

-- | Direct port of the private @cropDyn@ helper in the MTL original.
-- Folds successive decoded 'QueryResult' firings through 'crop' against
-- the sampled current 'Query'.
cropDyn
  :: forall es q.
     ( Query q
     , Hold T :> es
     , Sample T :> es
     , Push T :> es
     )
  => Dynamic T q
  -> Event T (QueryResult q)
  -> Eff es (Dynamic T (QueryResult q))
cropDyn q = foldDyn (\(q', qr) v -> crop q' (qr `mappend` v)) mempty . attach (current q)
