module LRUBasicTest
  ( module LRUBasicTest
  ) where

import Control.Monad (void)
import Test.HUnit (Assertion)

import Util

unit_empty_LRU :: Assertion
unit_empty_LRU = withLRUCacheOfSize @Int @Int 1 do
  getRIO 1 @?== Nothing
  getRIO 2 @?== Nothing

unit_insert :: Assertion
unit_insert = withLRUCacheOfSize @Int @Int 1 do
  insertRIO 1 1

  getRIO 1 @?== Just 1
  getRIO 2 @?== Nothing

unit_change_value :: Assertion
unit_change_value = withLRUCacheOfSize @Int @Int 1 do
  insertRIO 1 1
  getRIO 1 @?== Just 1

  insertRIO 1 2
  getRIO 1 @?== Just 2

unit_oversize :: Assertion
unit_oversize = withLRUCacheOfSize @Int @Int 2 do
  insertRIO 1 1
  insertRIO 2 2

  getRIO 1 @?== Just 1
  getRIO 2 @?== Just 2

  insertRIO 3 3

  getRIO 1 @?== Nothing
  getRIO 2 @?== Just 2
  getRIO 3 @?== Just 3

unit_change_value_without_expiring :: Assertion
unit_change_value_without_expiring = withLRUCacheOfSize @Int @Int 2 do
  insertRIO 1 1
  insertRIO 2 2
  insertRIO 1 2

  getRIO 1 @?== Just 2
  getRIO 2 @?== Just 2

unit_value_expire :: Assertion
unit_value_expire = withLRUCacheOfSize @Int @Int 2 do
  insertRIO 1 1
  insertRIO 2 2

  void $ getRIO 1

  insertRIO 3 3

  getRIO 1 @?== Just 1
  getRIO 2 @?== Nothing
  getRIO 3 @?== Just 3

unit_leetcode :: Assertion
unit_leetcode = withLRUCacheOfSize @Int @Int 2 do
  insertRIO 1 1
  insertRIO 2 2
  getRIO 1 @?== Just 1

  insertRIO 3 3
  getRIO 2 @?== Nothing

  insertRIO 4 4

  getRIO 1 @?== Nothing
  getRIO 3 @?== Just 3
  getRIO 4 @?== Just 4
