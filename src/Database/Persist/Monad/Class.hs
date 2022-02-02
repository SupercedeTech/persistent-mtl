{-|
Module: Database.Persist.Monad.Class

Defines the 'MonadSqlQuery' type class that a monad can make an instance of
in order to interpret how to run a
'Database.Persist.Monad.SqlQueryRep.SqlQueryRep' sent by a lifted function from
@Database.Persist.Monad.Shim@.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Persist.Monad.Class
  ( MonadTransaction(..)
  , MonadSqlQuery(..)
  ) where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import Data.Kind (Type)
import Data.Typeable (Typeable)

import Database.Persist.Monad.SqlQueryRep (SqlQueryRep)

-- | The type-class for monads that can execute queries in a single transaction
class (Monad m, MonadSqlQuery (TransactionM m)) => MonadTransaction m  where
  type TransactionM m :: Type -> Type

  -- | Run all queries in the given action using the same database connection.
  withTransaction :: TransactionM m a -> m a

-- | The type-class for monads that can run persistent database queries.
class (Monad m) => MonadSqlQuery m where

  -- | Interpret the given SQL query operation.
  runQueryRep :: Typeable record => SqlQueryRep record a -> m a

{- Instances for common monad transformers -}

instance MonadTransaction m => MonadTransaction (Reader.ReaderT r m) where
  type TransactionM (Reader.ReaderT r m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Reader.ReaderT r m) where
  runQueryRep = lift . runQueryRep

instance MonadTransaction m => MonadTransaction (Except.ExceptT e m) where
  type TransactionM (Except.ExceptT e m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Except.ExceptT e m) where
  runQueryRep = lift . runQueryRep

instance MonadTransaction m => MonadTransaction (Identity.IdentityT m) where
  type TransactionM (Identity.IdentityT m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Identity.IdentityT m) where
  runQueryRep = lift . runQueryRep

instance MonadTransaction m => MonadTransaction (Maybe.MaybeT m) where
  type TransactionM (Maybe.MaybeT m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Maybe.MaybeT m) where
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadTransaction m) => MonadTransaction (RWS.Lazy.RWST r w s m) where
  type TransactionM (RWS.Lazy.RWST r w s m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Lazy.RWST r w s m) where
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadTransaction m) => MonadTransaction (RWS.Strict.RWST r w s m) where
  type TransactionM (RWS.Strict.RWST r w s m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Strict.RWST r w s m) where
  runQueryRep = lift . runQueryRep

instance MonadTransaction m => MonadTransaction (State.Lazy.StateT s m) where
  type TransactionM (State.Lazy.StateT s m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (State.Lazy.StateT s m) where
  runQueryRep = lift . runQueryRep

instance MonadTransaction m => MonadTransaction (State.Strict.StateT s m) where
  type TransactionM (State.Strict.StateT s m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadSqlQuery m => MonadSqlQuery (State.Strict.StateT s m) where
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadTransaction m) => MonadTransaction (Writer.Lazy.WriterT w m) where
  type TransactionM (Writer.Lazy.WriterT w m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Lazy.WriterT w m) where
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadTransaction m) => MonadTransaction (Writer.Strict.WriterT w m) where
  type TransactionM (Writer.Strict.WriterT w m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Strict.WriterT w m) where
  runQueryRep = lift . runQueryRep

