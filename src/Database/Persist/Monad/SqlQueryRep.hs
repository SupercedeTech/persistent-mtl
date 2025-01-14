{-|
Module: Database.Persist.Monad.SqlQueryRep

Defines the 'SqlQueryRep' data type that contains a constructor corresponding
to a @persistent@ function.

This file is autogenerated, to keep it in sync with
@Database.Persist.Monad.Shim@.
-}

{- THIS FILE IS AUTOGENERATED AND SHOULD NOT BE EDITED MANUALLY -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad.SqlQueryRep
  ( SqlQueryRep(..)
  , runSqlQueryRep
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Acquire (Acquire)
import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, eqT, typeRep, (:~:)(..))
import Data.Void (Void)
import Database.Persist.Sql as Persist hiding (pattern Update)
import GHC.Stack (HasCallStack)

{-# ANN module "HLint: ignore" #-}

-- | The data type containing a constructor for each persistent function we'd
-- like to lift into 'Database.Persist.Monad.MonadSqlQuery'.
--
-- The @record@ type parameter contains the 'PersistEntity' types used in a
-- given function.
--
-- We're using a free-monads-like technique here to allow us to introspect
-- persistent functions in 'Database.Persist.Monad.MonadSqlQuery', e.g. to
-- mock out persistent calls in tests.
data SqlQueryRep record a where
  -- | Constructor corresponding to 'Persist.get'
  Get
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Maybe record)

  -- | Constructor corresponding to 'Persist.getMany'
  GetMany
    :: (PersistRecordBackend record SqlBackend)
    => [Key record] -> SqlQueryRep record (Map (Key record) record)

  -- | Constructor corresponding to 'Persist.getJust'
  GetJust
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record record

  -- | Constructor corresponding to 'Persist.getJustEntity'
  GetJustEntity
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Entity record)

  -- | Constructor corresponding to 'Persist.getEntity'
  GetEntity
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Maybe (Entity record))

  -- | Constructor corresponding to 'Persist.belongsTo'
  BelongsTo
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Maybe (Key record2)) -> record1 -> SqlQueryRep (record1, record2) (Maybe record2)

  -- | Constructor corresponding to 'Persist.belongsToJust'
  BelongsToJust
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Key record2) -> record1 -> SqlQueryRep (record1, record2) record2

  -- | Constructor corresponding to 'Persist.insert'
  Insert
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Key record)

  -- | Constructor corresponding to 'Persist.insert_'
  Insert_
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.insertMany'
  InsertMany
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record [Key record]

  -- | Constructor corresponding to 'Persist.insertMany_'
  InsertMany_
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.insertEntityMany'
  InsertEntityMany
    :: (PersistRecordBackend record SqlBackend)
    => [Entity record] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.insertKey'
  InsertKey
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.repsert'
  Repsert
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.repsertMany'
  RepsertMany
    :: (PersistRecordBackend record SqlBackend)
    => [(Key record, record)] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.replace'
  Replace
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.delete'
  Delete
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.update'
  Update
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> [Update record] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.updateGet'
  UpdateGet
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> [Update record] -> SqlQueryRep record record

  -- | Constructor corresponding to 'Persist.insertEntity'
  InsertEntity
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Entity record)

  -- | Constructor corresponding to 'Persist.insertRecord'
  InsertRecord
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record record

  -- | Constructor corresponding to 'Persist.getBy'
  GetBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> SqlQueryRep record (Maybe (Entity record))

#if MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.getByValue'
  GetByValue
    :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record)
    => record -> SqlQueryRep record (Maybe (Entity record))
#endif

#if !MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.getByValue'
  GetByValue
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Entity record))
#endif

  -- | Constructor corresponding to 'Persist.checkUnique'
  CheckUnique
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Unique record))

#if MIN_VERSION_persistent(2,11,0)
  -- | Constructor corresponding to 'Persist.checkUniqueUpdateable'
  CheckUniqueUpdateable
    :: (PersistRecordBackend record SqlBackend)
    => Entity record -> SqlQueryRep record (Maybe (Unique record))
#endif

  -- | Constructor corresponding to 'Persist.deleteBy'
  DeleteBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.insertUnique'
  InsertUnique
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Key record))

#if MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.upsert'
  Upsert
    :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record)
    => record -> [Update record] -> SqlQueryRep record (Entity record)
#endif

#if !MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.upsert'
  Upsert
    :: (PersistRecordBackend record SqlBackend)
    => record -> [Update record] -> SqlQueryRep record (Entity record)
#endif

  -- | Constructor corresponding to 'Persist.upsertBy'
  UpsertBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> record -> [Update record] -> SqlQueryRep record (Entity record)

  -- | Constructor corresponding to 'Persist.putMany'
  PutMany
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record ()

#if MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.insertBy'
  InsertBy
    :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record)
    => record -> SqlQueryRep record (Either (Entity record) (Key record))
#endif

#if !MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.insertBy'
  InsertBy
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Either (Entity record) (Key record))
#endif

  -- | Constructor corresponding to 'Persist.insertUniqueEntity'
  InsertUniqueEntity
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Entity record))

  -- | Constructor corresponding to 'Persist.replaceUnique'
  ReplaceUnique
    :: (PersistRecordBackend record SqlBackend, Eq (Unique record), Eq record)
    => Key record -> record -> SqlQueryRep record (Maybe (Unique record))

#if MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.onlyUnique'
  OnlyUnique
    :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record)
    => record -> SqlQueryRep record (Unique record)
#endif

#if !MIN_VERSION_persistent(2,10,0)
  -- | Constructor corresponding to 'Persist.onlyUnique'
  OnlyUnique
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Unique record)
#endif

  -- | Constructor corresponding to 'Persist.selectSourceRes'
  SelectSourceRes
    :: (MonadIO m2, PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record (Acquire (ConduitM () (Entity record) m2 ()))

  -- | Constructor corresponding to 'Persist.selectFirst'
  SelectFirst
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record (Maybe (Entity record))

  -- | Constructor corresponding to 'Persist.selectKeysRes'
  SelectKeysRes
    :: (MonadIO m2, PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record (Acquire (ConduitM () (Key record) m2 ()))

  -- | Constructor corresponding to 'Persist.count'
  Count
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Int

#if MIN_VERSION_persistent(2,11,0)
  -- | Constructor corresponding to 'Persist.exists'
  Exists
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Bool
#endif

  -- | Constructor corresponding to 'Persist.selectList'
  SelectList
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record [Entity record]

  -- | Constructor corresponding to 'Persist.selectKeysList'
  SelectKeysList
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record [Key record]

  -- | Constructor corresponding to 'Persist.updateWhere'
  UpdateWhere
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [Update record] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.deleteWhere'
  DeleteWhere
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record ()

  -- | Constructor corresponding to 'Persist.deleteWhereCount'
  DeleteWhereCount
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Int64

  -- | Constructor corresponding to 'Persist.updateWhereCount'
  UpdateWhereCount
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [Update record] -> SqlQueryRep record Int64

#if !MIN_VERSION_persistent(2,13,0)
  -- | Constructor corresponding to 'Persist.deleteCascade'
  DeleteCascade
    :: (DeleteCascade record SqlBackend)
    => Key record -> SqlQueryRep record ()
#endif

#if !MIN_VERSION_persistent(2,13,0)
  -- | Constructor corresponding to 'Persist.deleteCascadeWhere'
  DeleteCascadeWhere
    :: (DeleteCascade record SqlBackend)
    => [Filter record] -> SqlQueryRep record ()
#endif

  -- | Constructor corresponding to 'Persist.parseMigration'
  ParseMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void (Either [Text] CautiousMigration)

  -- | Constructor corresponding to 'Persist.parseMigration''
  ParseMigration'
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void CautiousMigration

  -- | Constructor corresponding to 'Persist.printMigration'
  PrintMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void ()

  -- | Constructor corresponding to 'Persist.showMigration'
  ShowMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Text]

  -- | Constructor corresponding to 'Persist.getMigration'
  GetMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Sql]

  -- | Constructor corresponding to 'Persist.runMigration'
  RunMigration
    :: ()
    => Migration -> SqlQueryRep Void ()

#if MIN_VERSION_persistent(2,10,2)
  -- | Constructor corresponding to 'Persist.runMigrationQuiet'
  RunMigrationQuiet
    :: ()
    => Migration -> SqlQueryRep Void [Text]
#endif

  -- | Constructor corresponding to 'Persist.runMigrationSilent'
  RunMigrationSilent
    :: ()
    => Migration -> SqlQueryRep Void [Text]

  -- | Constructor corresponding to 'Persist.runMigrationUnsafe'
  RunMigrationUnsafe
    :: ()
    => Migration -> SqlQueryRep Void ()

#if MIN_VERSION_persistent(2,10,2)
  -- | Constructor corresponding to 'Persist.runMigrationUnsafeQuiet'
  RunMigrationUnsafeQuiet
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Text]
#endif

  -- | Constructor corresponding to 'Persist.getFieldName'
  GetFieldName
    :: (PersistRecordBackend record SqlBackend)
    => EntityField record typ -> SqlQueryRep record Text

  -- | Constructor corresponding to 'Persist.getTableName'
  GetTableName
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record Text

  -- | Constructor corresponding to 'Persist.withRawQuery'
  WithRawQuery
    :: ()
    => Text -> [PersistValue] -> ConduitM [PersistValue] Void IO a -> SqlQueryRep Void a

  -- | Constructor corresponding to 'Persist.rawQueryRes'
  RawQueryRes
    :: (MonadIO m2)
    => Text -> [PersistValue] -> SqlQueryRep Void (Acquire (ConduitM () [PersistValue] m2 ()))

  -- | Constructor corresponding to 'Persist.rawExecute'
  RawExecute
    :: ()
    => Text -> [PersistValue] -> SqlQueryRep Void ()

  -- | Constructor corresponding to 'Persist.rawExecuteCount'
  RawExecuteCount
    :: ()
    => Text -> [PersistValue] -> SqlQueryRep Void Int64

  -- | Constructor corresponding to 'Persist.rawSql'
  RawSql
    :: (RawSql a)
    => Text -> [PersistValue] -> SqlQueryRep Void [a]

  -- | Constructor corresponding to 'Persist.transactionSave'
  TransactionSave
    :: ()
    => SqlQueryRep Void ()

#if MIN_VERSION_persistent(2,9,0)
  -- | Constructor corresponding to 'Persist.transactionSaveWithIsolation'
  TransactionSaveWithIsolation
    :: ()
    => IsolationLevel -> SqlQueryRep Void ()
#endif

  -- | Constructor corresponding to 'Persist.transactionUndo'
  TransactionUndo
    :: ()
    => SqlQueryRep Void ()

#if MIN_VERSION_persistent(2,9,0)
  -- | Constructor corresponding to 'Persist.transactionUndoWithIsolation'
  TransactionUndoWithIsolation
    :: ()
    => IsolationLevel -> SqlQueryRep Void ()
#endif

  -- | Constructor for lifting an arbitrary SqlPersistT action into SqlQueryRep.
  UnsafeLiftSql
    :: Text -> (forall m. MonadIO m => Persist.SqlPersistT m a) -> SqlQueryRep Void a

instance Typeable record => Show (SqlQueryRep record a) where
  show = \case
    Get{} -> "Get{..}" ++ record
    GetMany{} -> "GetMany{..}" ++ record
    GetJust{} -> "GetJust{..}" ++ record
    GetJustEntity{} -> "GetJustEntity{..}" ++ record
    GetEntity{} -> "GetEntity{..}" ++ record
    BelongsTo{} -> "BelongsTo{..}" ++ record
    BelongsToJust{} -> "BelongsToJust{..}" ++ record
    Insert{} -> "Insert{..}" ++ record
    Insert_{} -> "Insert_{..}" ++ record
    InsertMany{} -> "InsertMany{..}" ++ record
    InsertMany_{} -> "InsertMany_{..}" ++ record
    InsertEntityMany{} -> "InsertEntityMany{..}" ++ record
    InsertKey{} -> "InsertKey{..}" ++ record
    Repsert{} -> "Repsert{..}" ++ record
    RepsertMany{} -> "RepsertMany{..}" ++ record
    Replace{} -> "Replace{..}" ++ record
    Delete{} -> "Delete{..}" ++ record
    Update{} -> "Update{..}" ++ record
    UpdateGet{} -> "UpdateGet{..}" ++ record
    InsertEntity{} -> "InsertEntity{..}" ++ record
    InsertRecord{} -> "InsertRecord{..}" ++ record
    GetBy{} -> "GetBy{..}" ++ record
#if MIN_VERSION_persistent(2,10,0)
    GetByValue{} -> "GetByValue{..}" ++ record
#endif
#if !MIN_VERSION_persistent(2,10,0)
    GetByValue{} -> "GetByValue{..}" ++ record
#endif
    CheckUnique{} -> "CheckUnique{..}" ++ record
#if MIN_VERSION_persistent(2,11,0)
    CheckUniqueUpdateable{} -> "CheckUniqueUpdateable{..}" ++ record
#endif
    DeleteBy{} -> "DeleteBy{..}" ++ record
    InsertUnique{} -> "InsertUnique{..}" ++ record
#if MIN_VERSION_persistent(2,10,0)
    Upsert{} -> "Upsert{..}" ++ record
#endif
#if !MIN_VERSION_persistent(2,10,0)
    Upsert{} -> "Upsert{..}" ++ record
#endif
    UpsertBy{} -> "UpsertBy{..}" ++ record
    PutMany{} -> "PutMany{..}" ++ record
#if MIN_VERSION_persistent(2,10,0)
    InsertBy{} -> "InsertBy{..}" ++ record
#endif
#if !MIN_VERSION_persistent(2,10,0)
    InsertBy{} -> "InsertBy{..}" ++ record
#endif
    InsertUniqueEntity{} -> "InsertUniqueEntity{..}" ++ record
    ReplaceUnique{} -> "ReplaceUnique{..}" ++ record
#if MIN_VERSION_persistent(2,10,0)
    OnlyUnique{} -> "OnlyUnique{..}" ++ record
#endif
#if !MIN_VERSION_persistent(2,10,0)
    OnlyUnique{} -> "OnlyUnique{..}" ++ record
#endif
    SelectSourceRes{} -> "SelectSourceRes{..}" ++ record
    SelectFirst{} -> "SelectFirst{..}" ++ record
    SelectKeysRes{} -> "SelectKeysRes{..}" ++ record
    Count{} -> "Count{..}" ++ record
#if MIN_VERSION_persistent(2,11,0)
    Exists{} -> "Exists{..}" ++ record
#endif
    SelectList{} -> "SelectList{..}" ++ record
    SelectKeysList{} -> "SelectKeysList{..}" ++ record
    UpdateWhere{} -> "UpdateWhere{..}" ++ record
    DeleteWhere{} -> "DeleteWhere{..}" ++ record
    DeleteWhereCount{} -> "DeleteWhereCount{..}" ++ record
    UpdateWhereCount{} -> "UpdateWhereCount{..}" ++ record
#if !MIN_VERSION_persistent(2,13,0)
    DeleteCascade{} -> "DeleteCascade{..}" ++ record
#endif
#if !MIN_VERSION_persistent(2,13,0)
    DeleteCascadeWhere{} -> "DeleteCascadeWhere{..}" ++ record
#endif
    ParseMigration{} -> "ParseMigration{..}" ++ record
    ParseMigration'{} -> "ParseMigration'{..}" ++ record
    PrintMigration{} -> "PrintMigration{..}" ++ record
    ShowMigration{} -> "ShowMigration{..}" ++ record
    GetMigration{} -> "GetMigration{..}" ++ record
    RunMigration{} -> "RunMigration{..}" ++ record
#if MIN_VERSION_persistent(2,10,2)
    RunMigrationQuiet{} -> "RunMigrationQuiet{..}" ++ record
#endif
    RunMigrationSilent{} -> "RunMigrationSilent{..}" ++ record
    RunMigrationUnsafe{} -> "RunMigrationUnsafe{..}" ++ record
#if MIN_VERSION_persistent(2,10,2)
    RunMigrationUnsafeQuiet{} -> "RunMigrationUnsafeQuiet{..}" ++ record
#endif
    GetFieldName{} -> "GetFieldName{..}" ++ record
    GetTableName{} -> "GetTableName{..}" ++ record
    WithRawQuery{} -> "WithRawQuery{..}" ++ record
    RawQueryRes{} -> "RawQueryRes{..}" ++ record
    RawExecute{} -> "RawExecute{..}" ++ record
    RawExecuteCount{} -> "RawExecuteCount{..}" ++ record
    RawSql{} -> "RawSql{..}" ++ record
    TransactionSave{} -> "TransactionSave{..}" ++ record
#if MIN_VERSION_persistent(2,9,0)
    TransactionSaveWithIsolation{} -> "TransactionSaveWithIsolation{..}" ++ record
#endif
    TransactionUndo{} -> "TransactionUndo{..}" ++ record
#if MIN_VERSION_persistent(2,9,0)
    TransactionUndoWithIsolation{} -> "TransactionUndoWithIsolation{..}" ++ record
#endif
    UnsafeLiftSql label _ -> "UnsafeLiftSql{" ++ Text.unpack label ++ "}"
    where
      record = case recordTypeRep of
        Just recordType -> "<" ++ show recordType ++ ">"
        Nothing -> ""
      recordTypeRep = case eqT @record @Void of
        Just Refl -> Nothing
        Nothing -> Just $ typeRep $ Proxy @record

-- | A helper to execute the actual @persistent@ function corresponding to
-- each 'SqlQueryRep' data constructor.
runSqlQueryRep :: MonadUnliftIO m => SqlQueryRep record a -> Persist.SqlPersistT m a
runSqlQueryRep = \case
  Get a1 -> Persist.get a1
  GetMany a1 -> Persist.getMany a1
  GetJust a1 -> Persist.getJust a1
  GetJustEntity a1 -> Persist.getJustEntity a1
  GetEntity a1 -> Persist.getEntity a1
  BelongsTo a1 a2 -> Persist.belongsTo a1 a2
  BelongsToJust a1 a2 -> Persist.belongsToJust a1 a2
  Insert a1 -> Persist.insert a1
  Insert_ a1 -> Persist.insert_ a1
  InsertMany a1 -> Persist.insertMany a1
  InsertMany_ a1 -> Persist.insertMany_ a1
  InsertEntityMany a1 -> Persist.insertEntityMany a1
  InsertKey a1 a2 -> Persist.insertKey a1 a2
  Repsert a1 a2 -> Persist.repsert a1 a2
  RepsertMany a1 -> Persist.repsertMany a1
  Replace a1 a2 -> Persist.replace a1 a2
  Delete a1 -> Persist.delete a1
  Update a1 a2 -> Persist.update a1 a2
  UpdateGet a1 a2 -> Persist.updateGet a1 a2
  InsertEntity a1 -> Persist.insertEntity a1
  InsertRecord a1 -> Persist.insertRecord a1
  GetBy a1 -> Persist.getBy a1
#if MIN_VERSION_persistent(2,10,0)
  GetByValue a1 -> Persist.getByValue a1
#endif
#if !MIN_VERSION_persistent(2,10,0)
  GetByValue a1 -> Persist.getByValue a1
#endif
  CheckUnique a1 -> Persist.checkUnique a1
#if MIN_VERSION_persistent(2,11,0)
  CheckUniqueUpdateable a1 -> Persist.checkUniqueUpdateable a1
#endif
  DeleteBy a1 -> Persist.deleteBy a1
  InsertUnique a1 -> Persist.insertUnique a1
#if MIN_VERSION_persistent(2,10,0)
  Upsert a1 a2 -> Persist.upsert a1 a2
#endif
#if !MIN_VERSION_persistent(2,10,0)
  Upsert a1 a2 -> Persist.upsert a1 a2
#endif
  UpsertBy a1 a2 a3 -> Persist.upsertBy a1 a2 a3
  PutMany a1 -> Persist.putMany a1
#if MIN_VERSION_persistent(2,10,0)
  InsertBy a1 -> Persist.insertBy a1
#endif
#if !MIN_VERSION_persistent(2,10,0)
  InsertBy a1 -> Persist.insertBy a1
#endif
  InsertUniqueEntity a1 -> Persist.insertUniqueEntity a1
  ReplaceUnique a1 a2 -> Persist.replaceUnique a1 a2
#if MIN_VERSION_persistent(2,10,0)
  OnlyUnique a1 -> Persist.onlyUnique a1
#endif
#if !MIN_VERSION_persistent(2,10,0)
  OnlyUnique a1 -> Persist.onlyUnique a1
#endif
  SelectSourceRes a1 a2 -> Persist.selectSourceRes a1 a2
  SelectFirst a1 a2 -> Persist.selectFirst a1 a2
  SelectKeysRes a1 a2 -> Persist.selectKeysRes a1 a2
  Count a1 -> Persist.count a1
#if MIN_VERSION_persistent(2,11,0)
  Exists a1 -> Persist.exists a1
#endif
  SelectList a1 a2 -> Persist.selectList a1 a2
  SelectKeysList a1 a2 -> Persist.selectKeysList a1 a2
  UpdateWhere a1 a2 -> Persist.updateWhere a1 a2
  DeleteWhere a1 -> Persist.deleteWhere a1
  DeleteWhereCount a1 -> Persist.deleteWhereCount a1
  UpdateWhereCount a1 a2 -> Persist.updateWhereCount a1 a2
#if !MIN_VERSION_persistent(2,13,0)
  DeleteCascade a1 -> Persist.deleteCascade a1
#endif
#if !MIN_VERSION_persistent(2,13,0)
  DeleteCascadeWhere a1 -> Persist.deleteCascadeWhere a1
#endif
  ParseMigration a1 -> Persist.parseMigration a1
  ParseMigration' a1 -> Persist.parseMigration' a1
  PrintMigration a1 -> Persist.printMigration a1
  ShowMigration a1 -> Persist.showMigration a1
  GetMigration a1 -> Persist.getMigration a1
  RunMigration a1 -> Persist.runMigration a1
#if MIN_VERSION_persistent(2,10,2)
  RunMigrationQuiet a1 -> Persist.runMigrationQuiet a1
#endif
  RunMigrationSilent a1 -> Persist.runMigrationSilent a1
  RunMigrationUnsafe a1 -> Persist.runMigrationUnsafe a1
#if MIN_VERSION_persistent(2,10,2)
  RunMigrationUnsafeQuiet a1 -> Persist.runMigrationUnsafeQuiet a1
#endif
  GetFieldName a1 -> Persist.getFieldName a1
  GetTableName a1 -> Persist.getTableName a1
  WithRawQuery a1 a2 a3 -> Persist.withRawQuery a1 a2 a3
  RawQueryRes a1 a2 -> Persist.rawQueryRes a1 a2
  RawExecute a1 a2 -> Persist.rawExecute a1 a2
  RawExecuteCount a1 a2 -> Persist.rawExecuteCount a1 a2
  RawSql a1 a2 -> Persist.rawSql a1 a2
  TransactionSave -> Persist.transactionSave
#if MIN_VERSION_persistent(2,9,0)
  TransactionSaveWithIsolation a1 -> Persist.transactionSaveWithIsolation a1
#endif
  TransactionUndo -> Persist.transactionUndo
#if MIN_VERSION_persistent(2,9,0)
  TransactionUndoWithIsolation a1 -> Persist.transactionUndoWithIsolation a1
#endif
  UnsafeLiftSql _ action -> action
