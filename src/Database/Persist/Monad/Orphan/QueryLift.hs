{-|
Module: Database.Persist.Monad.Orphan.QueryLift

Instances for common monad transformers -}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Monad.Orphan.QueryLift () where

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

instance MonadQuery m => MonadQuery (Reader.ReaderT r m) where
  type Backend (Reader.ReaderT r m) = Backend m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Except.ExceptT e m) where
  type Backend (Except.ExceptT e m) = Backend m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Identity.IdentityT m) where
  type Backend (Identity.IdentityT m) = Backend m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Maybe.MaybeT m) where
  type Backend (Maybe.MaybeT m) = Backend m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (RWS.Lazy.RWST r w s m) where
  type Backend (RWS.Lazy.RWST r w s m) = Backend m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (RWS.Strict.RWST r w s m) where
  type Backend (RWS.Strict.RWST r w s m) = Backend m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (State.Lazy.StateT s m) where
  type Backend (State.Lazy.StateT s m) = Backend m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (State.Strict.StateT s m) where
  type Backend (State.Strict.StateT s m) = Backend m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (Writer.Lazy.WriterT w m) where
  type Backend (Writer.Lazy.WriterT w m) = Backend m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (Writer.Strict.WriterT w m) where
  type Backend (Writer.Strict.WriterT w m) = Backend m
  runQueryRep = lift . runQueryRep
