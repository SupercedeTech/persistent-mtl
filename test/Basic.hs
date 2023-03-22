{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Basic where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

tests :: TestTree
tests = testGroup "Basic functionality tests"
  [ testCase "Ensure withTransaction README example typechecks" $ do
      let foo :: MonadSqlQuery m => m ()
          foo = insert_ $ person "Alice"
          bar :: MonadSqlQuery m => m ()
          bar = insert_ $ person "Bob"
          fooAndBar :: MonadSqlTransaction m => m ()
          fooAndBar = withTransaction $ foo >> bar
      runMockSqlQueryT fooAndBar
        [ withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        , withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        ]
  ]
