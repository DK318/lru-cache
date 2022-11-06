module Util
  ( RIO
  , insertRIO
  , getRIO
  , withLRUCacheOfSize
  , (@?==)
  ) where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import GHC.Stack (HasCallStack)
import LRU (LRU, getValue, insert, unsafeMkLRU)

import Test.HUnit.Base qualified as HUnit

type RIO key value = ReaderT (IORef (LRU key value)) IO

insertRIO :: (Hashable key, Eq key) => key -> value -> RIO key value ()
insertRIO key value = do
  lruRef <- ask
  liftIO $ modifyIORef lruRef $ insert key value

getRIO :: (Hashable key, Eq key) => key -> RIO key value (Maybe value)
getRIO key = do
  lru <- ask >>= liftIO . readIORef
  let (res, newLru) = getValue key lru
  ask >>= liftIO . flip writeIORef newLru
  pure res

withLRUCacheOfSize :: Int -> RIO key value a -> IO a
withLRUCacheOfSize sz act = runReaderT act =<< newIORef (unsafeMkLRU sz)

(@?==) :: (HasCallStack, Eq a, Show a) => RIO key value a -> a -> RIO key value ()
ma @?== b = ma >>= \a -> liftIO $ a HUnit.@?= b
