{- THIS FILE IS AUTOGENERATED AND SHOULD NOT BE EDITED MANUALLY -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Generated where

import Data.Acquire (Acquire)
import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)
import Database.Persist.Sql (CautiousMigration, Entity, Key, PersistValue, Sql)

import Database.Persist.Monad.TestUtils (QueryRep(..), SqlQueryRep)
import Example

{-# ANN module "HLint: ignore" #-}

allSqlQueryRepShowRepresentations :: [String]
allSqlQueryRepShowRepresentations =
  [ show (Get undefined :: SqlQueryRep Person (Maybe Person))
  , show (GetMany undefined :: SqlQueryRep Person (Map (Key Person) Person))
  , show (GetJust undefined :: SqlQueryRep Person Person)
  , show (GetJustEntity undefined :: SqlQueryRep Person (Entity Person))
  , show (GetEntity undefined :: SqlQueryRep Person (Maybe (Entity Person)))
  , show (BelongsTo undefined undefined :: SqlQueryRep (Person, Post) (Maybe Post))
  , show (BelongsToJust undefined undefined :: SqlQueryRep (Person, Post) Post)
  , show (Insert undefined :: SqlQueryRep Person (Key Person))
  , show (Insert_ undefined :: SqlQueryRep Person ())
  , show (InsertMany undefined :: SqlQueryRep Person [Key Person])
  , show (InsertMany_ undefined :: SqlQueryRep Person ())
  , show (InsertEntityMany undefined :: SqlQueryRep Person ())
  , show (InsertKey undefined undefined :: SqlQueryRep Person ())
  , show (Repsert undefined undefined :: SqlQueryRep Person ())
  , show (RepsertMany undefined :: SqlQueryRep Person ())
  , show (Replace undefined undefined :: SqlQueryRep Person ())
  , show (Delete undefined :: SqlQueryRep Person ())
  , show (Update undefined undefined :: SqlQueryRep Person ())
  , show (UpdateGet undefined undefined :: SqlQueryRep Person Person)
  , show (InsertEntity undefined :: SqlQueryRep Person (Entity Person))
  , show (InsertRecord undefined :: SqlQueryRep Person Person)
  , show (GetBy undefined :: SqlQueryRep Person (Maybe (Entity Person)))
#if MIN_VERSION_persistent(2,10,0)
  , show (GetByValue undefined :: SqlQueryRep Person (Maybe (Entity Person)))
#endif
#if !MIN_VERSION_persistent(2,10,0)
  , show (GetByValue undefined :: SqlQueryRep Person (Maybe (Entity Person)))
#endif
  , show (CheckUnique undefined :: SqlQueryRep Person (Maybe (Unique Person)))
#if MIN_VERSION_persistent(2,11,0)
  , show (CheckUniqueUpdateable undefined :: SqlQueryRep Person (Maybe (Unique Person)))
#endif
  , show (DeleteBy undefined :: SqlQueryRep Person ())
  , show (InsertUnique undefined :: SqlQueryRep Person (Maybe (Key Person)))
#if MIN_VERSION_persistent(2,10,0)
  , show (Upsert undefined undefined :: SqlQueryRep Person (Entity Person))
#endif
#if !MIN_VERSION_persistent(2,10,0)
  , show (Upsert undefined undefined :: SqlQueryRep Person (Entity Person))
#endif
  , show (UpsertBy undefined undefined undefined :: SqlQueryRep Person (Entity Person))
  , show (PutMany undefined :: SqlQueryRep Person ())
#if MIN_VERSION_persistent(2,10,0)
  , show (InsertBy undefined :: SqlQueryRep Person (Either (Entity Person) (Key Person)))
#endif
#if !MIN_VERSION_persistent(2,10,0)
  , show (InsertBy undefined :: SqlQueryRep Person (Either (Entity Person) (Key Person)))
#endif
  , show (InsertUniqueEntity undefined :: SqlQueryRep Person (Maybe (Entity Person)))
  , show (ReplaceUnique undefined undefined :: SqlQueryRep Person (Maybe (Unique Person)))
#if MIN_VERSION_persistent(2,10,0)
  , show (OnlyUnique undefined :: SqlQueryRep Person (Unique Person))
#endif
#if !MIN_VERSION_persistent(2,10,0)
  , show (OnlyUnique undefined :: SqlQueryRep Person (Unique Person))
#endif
  , show (SelectSourceRes undefined undefined :: SqlQueryRep Person (Acquire (ConduitM () (Entity Person) IO ())))
  , show (SelectFirst undefined undefined :: SqlQueryRep Person (Maybe (Entity Person)))
  , show (SelectKeysRes undefined undefined :: SqlQueryRep Person (Acquire (ConduitM () (Key Person) IO ())))
  , show (Count undefined :: SqlQueryRep Person Int)
#if MIN_VERSION_persistent(2,11,0)
  , show (Exists undefined :: SqlQueryRep Person Bool)
#endif
  , show (SelectList undefined undefined :: SqlQueryRep Person [Entity Person])
  , show (SelectKeysList undefined undefined :: SqlQueryRep Person [Key Person])
  , show (UpdateWhere undefined undefined :: SqlQueryRep Person ())
  , show (DeleteWhere undefined :: SqlQueryRep Person ())
  , show (DeleteWhereCount undefined :: SqlQueryRep Person Int64)
  , show (UpdateWhereCount undefined undefined :: SqlQueryRep Person Int64)
#if !MIN_VERSION_persistent(2,13,0)
  , show (DeleteCascade undefined :: SqlQueryRep Person ())
#endif
#if !MIN_VERSION_persistent(2,13,0)
  , show (DeleteCascadeWhere undefined :: SqlQueryRep Person ())
#endif
  , show (ParseMigration undefined :: SqlQueryRep Void (Either [Text] CautiousMigration))
  , show (ParseMigration' undefined :: SqlQueryRep Void CautiousMigration)
  , show (PrintMigration undefined :: SqlQueryRep Void ())
  , show (ShowMigration undefined :: SqlQueryRep Void [Text])
  , show (GetMigration undefined :: SqlQueryRep Void [Sql])
  , show (RunMigration undefined :: SqlQueryRep Void ())
#if MIN_VERSION_persistent(2,10,2)
  , show (RunMigrationQuiet undefined :: SqlQueryRep Void [Text])
#endif
  , show (RunMigrationSilent undefined :: SqlQueryRep Void [Text])
  , show (RunMigrationUnsafe undefined :: SqlQueryRep Void ())
#if MIN_VERSION_persistent(2,10,2)
  , show (RunMigrationUnsafeQuiet undefined :: SqlQueryRep Void [Text])
#endif
  , show (GetFieldName undefined :: SqlQueryRep Person Text)
  , show (GetTableName undefined :: SqlQueryRep Person Text)
  , show (WithRawQuery undefined undefined undefined :: SqlQueryRep Void a)
  , show (RawQueryRes undefined undefined :: SqlQueryRep Void (Acquire (ConduitM () [PersistValue] IO ())))
  , show (RawExecute undefined undefined :: SqlQueryRep Void ())
  , show (RawExecuteCount undefined undefined :: SqlQueryRep Void Int64)
  , show (RawSql undefined undefined :: SqlQueryRep Void [Entity Person])
  , show (TransactionSave :: SqlQueryRep Void ())
#if MIN_VERSION_persistent(2,9,0)
  , show (TransactionSaveWithIsolation undefined :: SqlQueryRep Void ())
#endif
  , show (TransactionUndo :: SqlQueryRep Void ())
#if MIN_VERSION_persistent(2,9,0)
  , show (TransactionUndoWithIsolation undefined :: SqlQueryRep Void ())
#endif
  ]
