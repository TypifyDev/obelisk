{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Jenga.ExecutableConfig.Inject where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Control.Monad (mapM_)
import Data.Semigroup ((<>))
#endif
#endif
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Effectful (Eff, (:>))
import Reflex.Effectful.DomHost hiding (value)

-- | Produces a @<script>@ tag with an @id@ attribute containing the key and
-- whose value is the provided configuration, encoded in base64.
injectPure
  :: forall t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es
     , t ~ DomBuilderTimelineE es
     , Reflex t
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> ByteString -> Eff es ()
injectPure key value =
  let attrs = ("type" =: "text/plain" <> "data-jenga-executable-config-inject-key" =: key <> "data-hydration-skip" =: "")
  in elAttr "script" attrs $ text (T.decodeUtf8 (B64.encode value))

-- | Produces injectable @<script>@ tags containing the configuration keys
-- (filepaths) and values.
injectExecutableConfigs
  :: forall t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es
     , t ~ DomBuilderTimelineE es
     , Reflex t
     , DomSpace (DomBuilderSpaceE es)
     )
  => Map Text ByteString -> Eff es ()
injectExecutableConfigs = mapM_ (uncurry (injectPure @t)) . Map.toList
