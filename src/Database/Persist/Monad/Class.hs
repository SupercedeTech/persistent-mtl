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

import Database.Persist.Monad.SqlQueryRep (QueryRepCompatible(..), SqlQueryRep)

-- | The type-class for monads that can execute queries in a single transaction
class (Monad m, MonadQuery (TransactionM m)) => MonadTransaction m  where
  type TransactionM m :: Type -> Type

  -- | Run all queries in the given action using the same database connection.
  withTransaction :: TransactionM m a -> m a

type MonadSqlTransaction m = (MonadTransaction m, QueryRepCompatible SqlQueryRep (QueryRep (TransactionM m)))

-- | The type-class for monads that can run persistent database queries.
class (Monad m) => MonadQuery m where
  type QueryRep m :: Type -> Type -> Type

  -- | Interpret the given query operation.
  runQueryRep :: Typeable record => QueryRep m record a -> m a

type MonadSqlQuery m = (MonadQuery m, QueryRepCompatible SqlQueryRep (QueryRep m))

runCompatibleQueryRep
  :: (MonadQuery m, QueryRepCompatible rep (QueryRep m), Typeable record)
  => rep record a
  -> m a
runCompatibleQueryRep = runQueryRep . projectQueryRep

-- | A helpful monad wrapper for running a particular DB function "via" a
-- compatible backend, rather than the current one.
--
-- 'MonadQuery' specifies a specific 'QueryRep' type for each monad. But
-- classy DB functions (like ones that constrain by 'MonadSqlQuery') allow for
-- any query representation, provided it is 'QueryRepCompatible' with the
-- query representation they actually want to use - like, say, 'SqlQueryRep'.
--
-- So in a function that wants
-- @('MonadQuery m', 'QueryRepCompatible' MyQueryRep ('QueryRep' m))@
-- if we try to call a function that constrains by
-- @('MonadQuery m', 'QueryRepCompatible' MyOtherRep ('QueryRep' m))@
-- we will be told that the compiler cannot determine
-- @'QueryRepCompatible' MyOtherRep ('QueryRep' m)@
-- even if 'MyQueryRep' and 'MyOtherRep' *are* compatible. In this case,
-- polymorphism is a problem: the type doesn't know that it should first
-- convert @'QueryRep' m@ to a @MyQueryRep@, and then @MyQueryRep@ to a
-- @MyOtherRep@. We have to guide it there, which 'runVia' lets us do.
--
-- In the above example, we would use @'runVia' \@MyQueryRep$ ...@ to call
-- the second function, to tell the compiler that we want to use the
-- compatibility between @MyQueryRep@ and @MyOtherRep@.
newtype Via (sub :: Type -> Type -> Type) m a = Via { runVia :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance
  ( MonadQuery m
  , QueryRepCompatible sub (QueryRep m)
  ) => MonadQuery (Via sub m) where
  type QueryRep (Via sub m) = sub
  runQueryRep = Via . runCompatibleQueryRep
