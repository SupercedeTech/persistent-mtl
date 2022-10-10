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
  type QueryRep (Reader.ReaderT r m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Except.ExceptT e m) where
  type QueryRep (Except.ExceptT e m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Identity.IdentityT m) where
  type QueryRep (Identity.IdentityT m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (Maybe.MaybeT m) where
  type QueryRep (Maybe.MaybeT m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (RWS.Lazy.RWST r w s m) where
  type QueryRep (RWS.Lazy.RWST r w s m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (RWS.Strict.RWST r w s m) where
  type QueryRep (RWS.Strict.RWST r w s m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (State.Lazy.StateT s m) where
  type QueryRep (State.Lazy.StateT s m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance MonadQuery m => MonadQuery (State.Strict.StateT s m) where
  type QueryRep (State.Strict.StateT s m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (Writer.Lazy.WriterT w m) where
  type QueryRep (Writer.Lazy.WriterT w m) = QueryRep m
  runQueryRep = lift . runQueryRep

instance (Monoid w, MonadQuery m) => MonadQuery (Writer.Strict.WriterT w m) where
  type QueryRep (Writer.Strict.WriterT w m) = QueryRep m
  runQueryRep = lift . runQueryRep
