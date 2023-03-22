{-|
Module: Database.Persist.Monad.Orphan.TransactionLift

Module of orphan instances which "lift" 'MonadTransaction' through various
monad transformers by simply not allowing those transformers' effects to
occur inside the transaction.

Most monad transformers cannot safely be made to lift 'MonadTransaction',
because they will allow for breaking the transaction semantics or transaction
re-running.

However, thanks to 'TransactionM' being defined per-instance, it's possible
to automatically lift "running a transaction" through any monad transformer
by limiting the effects inside the transaction, so 'TransactionM' is *not*
lifted.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Monad.Orphan.TransactionLift () where

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

import Database.Persist.Monad.Class

instance MonadTransaction m => MonadTransaction (Reader.ReaderT r m) where
  type TransactionM (Reader.ReaderT r m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadTransaction m => MonadTransaction (Except.ExceptT e m) where
  type TransactionM (Except.ExceptT e m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadTransaction m => MonadTransaction (Identity.IdentityT m) where
  type TransactionM (Identity.IdentityT m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadTransaction m => MonadTransaction (Maybe.MaybeT m) where
  type TransactionM (Maybe.MaybeT m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadTransaction m) => MonadTransaction (RWS.Lazy.RWST r w s m) where
  type TransactionM (RWS.Lazy.RWST r w s m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadTransaction m) => MonadTransaction (RWS.Strict.RWST r w s m) where
  type TransactionM (RWS.Strict.RWST r w s m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadTransaction m => MonadTransaction (State.Lazy.StateT s m) where
  type TransactionM (State.Lazy.StateT s m) = TransactionM m
  withTransaction = lift . withTransaction

instance MonadTransaction m => MonadTransaction (State.Strict.StateT s m) where
  type TransactionM (State.Strict.StateT s m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadTransaction m) => MonadTransaction (Writer.Lazy.WriterT w m) where
  type TransactionM (Writer.Lazy.WriterT w m) = TransactionM m
  withTransaction = lift . withTransaction

instance (Monoid w, MonadTransaction m) => MonadTransaction (Writer.Strict.WriterT w m) where
  type TransactionM (Writer.Strict.WriterT w m) = TransactionM m
  withTransaction = lift . withTransaction
