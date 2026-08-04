{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jenga.Frontend.Promise where

import Language.Javascript.JSaddle
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Lens ((^.))

newtype Promise =
  UnsafeToPromise JSVal
  deriving (MakeObject)

unsafeToPromise :: JSVal -> Promise
unsafeToPromise = UnsafeToPromise

handlePromise :: Promise -> (JSVal -> JSM b) -> (JSVal -> JSM a) -> JSM (Either a b)
handlePromise (UnsafeToPromise val_) thenHandler catchHandler = do
  mvar <- liftIO $ newEmptyMVar
  nextVal <- val_ ^. js1 ("then" :: JSString) (fun $ \_ _ v -> case v of { (x:_) -> thenHandler x >>= liftIO . putMVar mvar . Right ; [] -> pure () })
  _ <- nextVal ^. js1 ("catch" :: JSString) (fun $ \_ _ err -> case err of { (x:_) -> catchHandler x >>= liftIO . putMVar mvar . Left ; [] -> pure () })
  liftIO $ takeMVar mvar

promiseMaybe :: Promise -> (JSVal -> JSM a) -> JSM (Maybe a)
promiseMaybe promise thenHandler = do
  eitherToMaybe <$> handlePromise promise thenHandler (\_ -> pure ())
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Right x_) = Just x_
    eitherToMaybe _ = Nothing
