{-|
Module: Database.Persist.Monad.Class

Defines the 'MonadSqlQuery' type class that a monad can make an instance of
in order to interpret how to run a
'Database.Persist.Monad.SqlQueryRep.SqlQueryRep' sent by a lifted function from
@Database.Persist.Monad.Shim@.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , Via(..)
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

-- | A helpful monad wrapper for running a particular DB function "via" a
-- compatible backend, rather than the current one.
--
-- 'MonadQuery' specifies a specific 'Backend' type for each monad. But
-- classy DB functions (like ones that constrain by 'MonadSqlQuery') allow for
-- any backend type, provided it is 'QueryRepCompatible' with the backend
-- they actually want to use the features of - like, say, 'SqlBackend'.
--
-- So in a function that wants
-- @('MonadQuery m', 'QueryRepCompatible' MyBackend ('Backend' m))@
-- if we try to call a function that constrains by
-- @('MonadQuery m', 'QueryRepCompatible' MyOtherBackend ('Backend' m))@
-- we will be told that the compiler cannot determine
-- @'QueryRepCompatible' MyOtherBackend ('Backend' m)@
-- even if 'MyBackend' and 'MyOtherBackend' *are* compatible. In this case,
-- polymorphism is a problem: the type doesn't know that it should first
-- convert @'Backend' m@ to a @MyBackend@, and then @MyBackend@ to a
-- @MyOtherBackend@. We have to guide it there, which 'runVia' lets us do.
--
-- In the above example, we would use @'runVia' \@MyBackend $ ...@ to call
-- the second function, to tell the compiler that we want to use the
-- compatibility between @MyBackend@ and @MyOtherBackend@.
newtype Via sub m a = Via { runVia :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance
  ( MonadQuery m
  , QueryRepresentable sub
  , QueryRepCompatible sub (Backend m)
  ) => MonadQuery (Via sub m) where
  type Backend (Via sub m) = sub
  runQueryRep = Via . runCompatibleQueryRep
