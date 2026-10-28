{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | MTL application widgets hosted by Jenga's Effectful DOM interpreters.
module Jenga.Frontend.MTL
  ( FrontendBaseMTL, FrontendWidgetMTL, FrontendMTLWidget
  , runFrontendMTLWidget
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Monoid (Endo)
import Data.Text (Text)
import Jenga.Configs.MTL (HasConfigs, runConfigsT)
import Jenga.Route.FrontendMTL
  ( RoutedT, RouteToUrl, SetRoute, runRoutedT, runRouteToUrlT, runSetRouteT )
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)

type FrontendBaseMTL t m =
  ( DomBuilder t m, HasDocument m, Prerender t m, MonadFix m, MonadHold t m
  , PostBuild t m, PerformEvent t m, TriggerEvent t m
  , MonadSample t (Performable m), MonadReflexCreateTrigger t m
  , MonadIO m, MonadIO (Performable m), PrimMonad m
  , MonadRef m, Ref m ~ IORef
  , MonadRef (Performable m), Ref (Performable m) ~ IORef
  )

type FrontendWidgetMTL t r m =
  ( FrontendBaseMTL t m, HasConfigs m, HasConfigs (Client m)
  , RouteToUrl r m, SetRoute t r m
  , RouteToUrl r (Client m), SetRoute t r (Client m)
  )

-- | Cookies are the request/boot snapshot; configuration and routing retain
-- their MTL interfaces throughout child widgets and prerender branches.
type FrontendMTLWidget r = forall t m.
  FrontendWidgetMTL t r m => [(ByteString, ByteString)] -> RoutedT t r m ()

runFrontendMTLWidget
  :: FrontendBaseMTL t m
  => Map Text ByteString -> [(ByteString, ByteString)]
  -> Dynamic t r -> (r -> Text) -> FrontendMTLWidget r
  -> m ((), Event t (Endo r))
runFrontendMTLWidget configs cookies route encode widget =
  runConfigsT configs $ flip runRouteToUrlT encode $
    runSetRouteT $ runRoutedT (widget cookies) route
