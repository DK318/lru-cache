module Util
  ( whenJust
  , assertFromJust
  ) where

import Control.Exception (assert)
import Data.Maybe (fromJust, isJust)
import GHC.Stack (HasCallStack)

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust = \case
  Nothing -> const $ pure ()
  Just a -> \f -> f a

assertFromJust :: HasCallStack => Maybe a -> a
assertFromJust mb = assert (isJust mb) $ fromJust mb
