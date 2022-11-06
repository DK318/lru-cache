module LRU
  ( LRU
  , unsafeMkLRU
  , insert
  , getValue
  ) where

import Control.Exception (assert)
import Control.Lens
  (At (at), _Just, abbreviatedFields, makeLensesWith, use, (&), (.=), (.~), (?=), (?~), (^.), (^?))
import Control.Monad.State (State, execState)
import Data.HashMap.Strict (HashMap, (!?))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import GHC.Stack (HasCallStack)

import Util (assertFromJust, whenJust)

data LinkedValue key value = LinkedValue
  { lvValue :: value
  , lvPrev :: Maybe key -- ^ to head
  , lvNext :: Maybe key -- ^ to tail
  } deriving stock (Functor, Foldable, Traversable, Show)
makeLensesWith abbreviatedFields ''LinkedValue

data LRU key value = LRU
  { lruCapacity :: Int
  , lruFirstInL :: Maybe key -- ^ tail
  , lruLastInL :: Maybe key -- ^ head
  , lruMapContents :: HashMap key (LinkedValue key value)
  } deriving stock (Functor, Foldable, Traversable, Show)
makeLensesWith abbreviatedFields ''LRU

unsafeMkLRU :: Int -> LRU key value
unsafeMkLRU cap = assert (cap > 0) $ LRU
  { lruCapacity = cap
  , lruFirstInL = Nothing
  , lruLastInL = Nothing
  , lruMapContents = HM.empty
  }

insert :: (HasCallStack, Hashable key, Eq key) => key -> value -> LRU key value -> LRU key value
insert key val lru@LRU{..}
  | HM.null lruMapContents = LRU
      { lruFirstInL = Just key
      , lruLastInL = Just key
      , lruMapContents = HM.singleton key $ LinkedValue val Nothing Nothing
      , ..
      }
  | HM.size lruMapContents < lru ^. capacity =
      if key `HM.member` lruMapContents
      then lru
        & mapContents . at key . _Just . value .~ val
        & moveToTail key
      else
        let
          newNode = LinkedValue
            { lvValue = val
            , lvPrev = lruFirstInL
            , lvNext = Nothing
            }
        in lru
          & mapContents . at key ?~ newNode
          & moveToTail key
  | otherwise =
      let
        lastElem = assertFromJust lruLastInL
        lastElemNext = assertFromJust (lruMapContents ^? at lastElem . _Just . next)
      in lru
        & mapContents . at lastElem .~ Nothing
        & lastInL .~ lastElemNext
        & insert key val

getValue :: (Hashable key, Eq key) => key -> LRU key value -> (Maybe value, LRU key value)
getValue key lru@LRU{..} = case lruMapContents !? key of
  Nothing -> (Nothing, lru)
  Just LinkedValue{..} -> (Just lvValue, moveToTail key lru)

moveToTail :: forall key value. (HasCallStack, Hashable key, Eq key) => key -> LRU key value -> LRU key value
moveToTail key lru@LRU{..}
  | HM.size lruMapContents <= 1 = lru
  | otherwise =  ensureElement key $ execState go lru
  where
    ensureElement :: key -> a -> a
    ensureElement key' = assert (key' `HM.member` lruMapContents)

    go :: State (LRU key value) ()
    go = do
      node <- assertFromJust <$> use (mapContents . at key)
      let leftMb = node ^. prev
      let rightMb = node ^. next

      case leftMb of
        Just left -> do
          ensureElement left $ mapContents . at left . _Just . next .= rightMb
        Nothing -> do
          lastInL .= rightMb

      whenJust rightMb \right -> do
        ensureElement right $ mapContents . at right . _Just . prev .= leftMb

      prevFirstInL <- assertFromJust <$> use firstInL
      mapContents . at prevFirstInL . _Just . next ?= key
      firstInL ?= key

      let node' = node
            & next .~ Nothing
            & prev ?~ prevFirstInL

      mapContents . at key ?= node'
