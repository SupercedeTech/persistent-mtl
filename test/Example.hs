{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Example
  ( TestApp
  , runTestApp

    -- * Person
  , Person(..)
  , person
  , getPeople
  , getPeopleNames
  , getName
  , nameAndAge

    -- * Post
  , Post(..)
  , post
  , getPosts
  , getPostTitles

    -- * Persistent
  , EntityField(..)
  , Unique(..)
  , migration
  ) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Data.Text as Text
import Database.Persist.Sql (Entity(..), EntityField, Key, Unique, toSqlKey)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH
    ( mkDeleteCascade
    , mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import UnliftIO (MonadUnliftIO(..), withSystemTempDirectory, wrappedWithRunInIO)

import Database.Persist.Monad

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migration"
  ]
  [persistLowerCase|
Person
  name String
  age Int
  UniqueName name
  deriving Show Eq

Post
  title String
  author PersonId
  editor PersonId Maybe
  deriving Show Eq
|]

deriving instance Eq (Unique Person)
#if !MIN_VERSION_persistent_template(2,6,0) || MIN_VERSION_persistent_template(2,9,0)
deriving instance Show (Unique Person)
#endif

-- Let tests use a literal number for keys
instance Num (Key Person) where
  fromInteger = toSqlKey . fromInteger

instance Num (Key Post) where
  fromInteger = toSqlKey . fromInteger

newtype TestApp a = TestApp
  { unTestApp :: SqlQueryT (ResourceT IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadSqlQuery
    , MonadResource
    )

instance MonadUnliftIO TestApp where
  withRunInIO = wrappedWithRunInIO TestApp unTestApp

runTestApp :: TestApp a -> IO a
runTestApp m =
  withSystemTempDirectory "persistent-mtl-testapp" $ \dir -> do
    let db = Text.pack $ dir ++ "/db.sqlite"
    runNoLoggingT $ withSqlitePool db 5 $ \pool ->
      liftIO . runResourceT . runSqlQueryT pool . unTestApp $ do
        _ <- runMigrationSilent migration
        m

{- Person functions -}

person :: String -> Person
person name = Person name 0

getName :: Entity Person -> String
getName = personName . entityVal

getPeople :: MonadSqlQuery m => m [Person]
getPeople = map entityVal <$> selectList [] []

getPeopleNames :: MonadSqlQuery m => m [String]
getPeopleNames = map personName <$> getPeople

nameAndAge :: Person -> (String, Int)
nameAndAge = personName &&& personAge

{- Post functions -}

post :: String -> Key Person -> Post
post title author = Post title author Nothing

getPosts :: MonadSqlQuery m => m [Post]
getPosts = map entityVal <$> selectList [] []

getPostTitles :: MonadSqlQuery m => m [String]
getPostTitles = map postTitle <$> getPosts
