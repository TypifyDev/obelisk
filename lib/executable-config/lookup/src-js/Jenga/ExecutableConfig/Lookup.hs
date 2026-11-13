{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Jenga.ExecutableConfig.Lookup where

import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHCJS.DOM
import "ghcjs-dom" GHCJS.DOM.Document (getHead)
import GHCJS.DOM.DOMStringMap (get)
import GHCJS.DOM.Element (getInnerHTML)
import GHCJS.DOM.HTMLElement (HTMLElement(HTMLElement), getDataset)
import GHCJS.DOM.NodeList (IsNodeList, item, getLength)
import GHCJS.DOM.ParentNode (querySelectorAll)
import GHCJS.DOM.Types (MonadJSM, Node(Node), castTo)
import Language.Javascript.JSaddle (JSM)

getConfigs :: JSM (Map Text ByteString)
getConfigs = getConfigsResult >>= either (liftIO . ioError . userError . show) pure

data ConfigLookupError
  = MissingDocument
  | MissingHead
  | InvalidConfigElement
  | InvalidConfigEncoding Text String
  deriving (Eq, Show)

-- | Preserve the complete typed ingestion result for callers that recover.
getConfigsResult :: JSM (Either ConfigLookupError (Map Text ByteString))
getConfigsResult = runExceptT $ do
  doc <- lift currentDocument >>= maybe (throwE MissingDocument) pure
  hd <- lift (getHead doc) >>= maybe (throwE MissingHead) pure
  nodes <- lift $ nodeListNodes =<< querySelectorAll hd ("[data-jenga-executable-config-inject-key]" :: Text)
  Map.fromList <$> traverse readConfigNode nodes

readConfigNode :: Node -> ExceptT ConfigLookupError JSM (Text, ByteString)
readConfigNode node = do
  element <- lift (castTo HTMLElement node) >>= maybe (throwE InvalidConfigElement) pure
  dataset <- lift $ getDataset element
  key <- lift $ get dataset ("jengaExecutableConfigInjectKey" :: Text)
  encoded <- lift $ getInnerHTML element
  decoded <- either (throwE . InvalidConfigEncoding key) pure $ B64.decode (T.encodeUtf8 encoded)
  pure (key, decoded)

-- | Collect all nodes in the node list.
--
-- TODO: this and the version in jenga-frontend should be
-- upstreamed to jsaddle.
nodeListNodes :: (IsNodeList l, MonadJSM m) => l -> m [Node]
nodeListNodes es = do
  len <- getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (item es) $ if len == 0 then [] else [0..len-1]
  pure $ catMaybes nodes
