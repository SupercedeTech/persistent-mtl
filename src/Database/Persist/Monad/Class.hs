{-|
Module: Database.Persist.Monad.Class

Defines the 'MonadSqlQuery' type class that a monad can make an instance of
in order to interpret how to run a
'Database.Persist.Monad.SqlQueryRep.SqlQueryRep' sent by a lifted function from
@Database.Persist.Monad.Shim@.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Persist.Monad.Class
  ( MonadTransaction(..)
  , MonadQuery(..)
  , MonadSqlTransaction
  , MonadSqlQuery
  , runCompatibleQueryRep
  ) where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Database.Persist.Sql

import Database.Persist.Monad.SqlQueryRep (QueryRepresentable(..), QueryRepCompatible(..))

-- | The type-class for monads that can execute queries in a single transaction
class (Monad m, MonadQuery (TransactionM m)) => MonadTransaction m  where
  type TransactionM m :: Type -> Type

  -- | Run all queries in the given action using the same database connection.
  withTransaction :: TransactionM m a -> m a

type MonadSqlTransaction m = (MonadTransaction m, QueryRepCompatible SqlBackend (Backend (TransactionM m)))

-- | The type-class for monads that can run persistent database queries.
class (Monad m, QueryRepresentable (Backend m)) => MonadQuery m where
  type Backend m :: Type

  -- | Interpret the given query operation.
  runQueryRep :: Typeable record => QueryRep (Backend m) record a -> m a

type MonadSqlQuery m = (MonadQuery m, QueryRepCompatible SqlBackend (Backend m))

runCompatibleQueryRep
  :: (MonadQuery m, QueryRepCompatible backend (Backend m), Typeable record)
  => QueryRep backend record a
  -> m a
runCompatibleQueryRep = runQueryRep . projectQueryRep
