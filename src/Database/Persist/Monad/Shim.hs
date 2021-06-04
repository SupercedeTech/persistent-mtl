{-|
Module: Database.Persist.Monad.Shim

Defines all the @persistent@ functions lifted into 'MonadSqlQuery'.

This file is autogenerated, to keep it in sync with
@Database.Persist.Monad.SqlQueryRep@.
-}

{- THIS FILE IS AUTOGENERATED AND SHOULD NOT BE EDITED MANUALLY -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Persist.Monad.Shim where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Acquire (Acquire, allocateAcquire)
import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Database.Persist.Sql hiding (pattern Update)
import GHC.Stack (HasCallStack)

import Database.Persist.Monad.Class (MonadSqlQuery(..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep(..))

{-# ANN module "HLint: ignore" #-}

-- | The lifted version of 'Database.Persist.Sql.get'
get
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Maybe record)
get a1 = runQueryRep $ Get a1

-- | The lifted version of 'Database.Persist.Sql.getMany'
getMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Key record] -> m (Map (Key record) record)
getMany a1 = runQueryRep $ GetMany a1

-- | The lifted version of 'Database.Persist.Sql.getJust'
getJust
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m record
getJust a1 = runQueryRep $ GetJust a1

-- | The lifted version of 'Database.Persist.Sql.getJustEntity'
getJustEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Entity record)
getJustEntity a1 = runQueryRep $ GetJustEntity a1

-- | The lifted version of 'Database.Persist.Sql.getEntity'
getEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Maybe (Entity record))
getEntity a1 = runQueryRep $ GetEntity a1

-- | The lifted version of 'Database.Persist.Sql.belongsTo'
belongsTo
  :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m)
  => (record1 -> Maybe (Key record2)) -> record1 -> m (Maybe record2)
belongsTo a1 a2 = runQueryRep $ BelongsTo a1 a2

-- | The lifted version of 'Database.Persist.Sql.belongsToJust'
belongsToJust
  :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m)
  => (record1 -> Key record2) -> record1 -> m record2
belongsToJust a1 a2 = runQueryRep $ BelongsToJust a1 a2

-- | The lifted version of 'Database.Persist.Sql.insert'
insert
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Key record)
insert a1 = runQueryRep $ Insert a1

-- | The lifted version of 'Database.Persist.Sql.insert_'
insert_
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m ()
insert_ a1 = runQueryRep $ Insert_ a1

-- | The lifted version of 'Database.Persist.Sql.insertMany'
insertMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m [Key record]
insertMany a1 = runQueryRep $ InsertMany a1

-- | The lifted version of 'Database.Persist.Sql.insertMany_'
insertMany_
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m ()
insertMany_ a1 = runQueryRep $ InsertMany_ a1

-- | The lifted version of 'Database.Persist.Sql.insertEntityMany'
insertEntityMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Entity record] -> m ()
insertEntityMany a1 = runQueryRep $ InsertEntityMany a1

-- | The lifted version of 'Database.Persist.Sql.insertKey'
insertKey
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
insertKey a1 a2 = runQueryRep $ InsertKey a1 a2

-- | The lifted version of 'Database.Persist.Sql.repsert'
repsert
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
repsert a1 a2 = runQueryRep $ Repsert a1 a2

-- | The lifted version of 'Database.Persist.Sql.repsertMany'
repsertMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [(Key record, record)] -> m ()
repsertMany a1 = runQueryRep $ RepsertMany a1

-- | The lifted version of 'Database.Persist.Sql.replace'
replace
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
replace a1 a2 = runQueryRep $ Replace a1 a2

-- | The lifted version of 'Database.Persist.Sql.delete'
delete
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m ()
delete a1 = runQueryRep $ Delete a1

-- | The lifted version of 'Database.Persist.Sql.update'
update
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> [Update record] -> m ()
update a1 a2 = runQueryRep $ Update a1 a2

-- | The lifted version of 'Database.Persist.Sql.updateGet'
updateGet
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> [Update record] -> m record
updateGet a1 a2 = runQueryRep $ UpdateGet a1 a2

-- | The lifted version of 'Database.Persist.Sql.insertEntity'
insertEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Entity record)
insertEntity a1 = runQueryRep $ InsertEntity a1

-- | The lifted version of 'Database.Persist.Sql.insertRecord'
insertRecord
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m record
insertRecord a1 = runQueryRep $ InsertRecord a1

-- | The lifted version of 'Database.Persist.Sql.getBy'
getBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> m (Maybe (Entity record))
getBy a1 = runQueryRep $ GetBy a1

#if MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.getByValue'
getByValue
  :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Entity record))
getByValue a1 = runQueryRep $ GetByValue a1
#endif

#if !MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.getByValue'
getByValue
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Entity record))
getByValue a1 = runQueryRep $ GetByValue a1
#endif

-- | The lifted version of 'Database.Persist.Sql.checkUnique'
checkUnique
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Unique record))
checkUnique a1 = runQueryRep $ CheckUnique a1

#if MIN_VERSION_persistent(2,11,0)
-- | The lifted version of 'Database.Persist.Sql.checkUniqueUpdateable'
checkUniqueUpdateable
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Entity record -> m (Maybe (Unique record))
checkUniqueUpdateable a1 = runQueryRep $ CheckUniqueUpdateable a1
#endif

-- | The lifted version of 'Database.Persist.Sql.deleteBy'
deleteBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> m ()
deleteBy a1 = runQueryRep $ DeleteBy a1

-- | The lifted version of 'Database.Persist.Sql.insertUnique'
insertUnique
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Key record))
insertUnique a1 = runQueryRep $ InsertUnique a1

#if MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.upsert'
upsert
  :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> [Update record] -> m (Entity record)
upsert a1 a2 = runQueryRep $ Upsert a1 a2
#endif

#if !MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.upsert'
upsert
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> [Update record] -> m (Entity record)
upsert a1 a2 = runQueryRep $ Upsert a1 a2
#endif

-- | The lifted version of 'Database.Persist.Sql.upsertBy'
upsertBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> record -> [Update record] -> m (Entity record)
upsertBy a1 a2 a3 = runQueryRep $ UpsertBy a1 a2 a3

-- | The lifted version of 'Database.Persist.Sql.putMany'
putMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m ()
putMany a1 = runQueryRep $ PutMany a1

#if MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.insertBy'
insertBy
  :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Either (Entity record) (Key record))
insertBy a1 = runQueryRep $ InsertBy a1
#endif

#if !MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.insertBy'
insertBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Either (Entity record) (Key record))
insertBy a1 = runQueryRep $ InsertBy a1
#endif

-- | The lifted version of 'Database.Persist.Sql.insertUniqueEntity'
insertUniqueEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Entity record))
insertUniqueEntity a1 = runQueryRep $ InsertUniqueEntity a1

-- | The lifted version of 'Database.Persist.Sql.replaceUnique'
replaceUnique
  :: (PersistRecordBackend record SqlBackend, Eq (Unique record), Eq record, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m (Maybe (Unique record))
replaceUnique a1 a2 = runQueryRep $ ReplaceUnique a1 a2

#if MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.onlyUnique'
onlyUnique
  :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Unique record)
onlyUnique a1 = runQueryRep $ OnlyUnique a1
#endif

#if !MIN_VERSION_persistent(2,10,0)
-- | The lifted version of 'Database.Persist.Sql.onlyUnique'
onlyUnique
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Unique record)
onlyUnique a1 = runQueryRep $ OnlyUnique a1
#endif

-- | The lifted version of 'Database.Persist.Sql.selectSourceRes'
selectSourceRes
  :: (MonadIO m2, PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m (Acquire (ConduitM () (Entity record) m2 ()))
selectSourceRes a1 a2 = runQueryRep $ SelectSourceRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.selectFirst'
selectFirst
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
selectFirst a1 a2 = runQueryRep $ SelectFirst a1 a2

-- | The lifted version of 'Database.Persist.Sql.selectKeysRes'
selectKeysRes
  :: (MonadIO m2, PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m (Acquire (ConduitM () (Key record) m2 ()))
selectKeysRes a1 a2 = runQueryRep $ SelectKeysRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.count'
count
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Int
count a1 = runQueryRep $ Count a1

#if MIN_VERSION_persistent(2,11,0)
-- | The lifted version of 'Database.Persist.Sql.exists'
exists
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Bool
exists a1 = runQueryRep $ Exists a1
#endif

-- | The lifted version of 'Database.Persist.Sql.selectSource'
selectSource
  :: (PersistRecordBackend record SqlBackend, MonadResource m, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> ConduitM () (Entity record) m ()
selectSource a1 a2 = fromAcquire $ runQueryRep $ SelectSourceRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.selectKeys'
selectKeys
  :: (PersistRecordBackend record SqlBackend, MonadResource m, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> ConduitM () (Key record) m ()
selectKeys a1 a2 = fromAcquire $ runQueryRep $ SelectKeysRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.selectList'
selectList
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m [Entity record]
selectList a1 a2 = runQueryRep $ SelectList a1 a2

-- | The lifted version of 'Database.Persist.Sql.selectKeysList'
selectKeysList
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m [Key record]
selectKeysList a1 a2 = runQueryRep $ SelectKeysList a1 a2

-- | The lifted version of 'Database.Persist.Sql.updateWhere'
updateWhere
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [Update record] -> m ()
updateWhere a1 a2 = runQueryRep $ UpdateWhere a1 a2

-- | The lifted version of 'Database.Persist.Sql.deleteWhere'
deleteWhere
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m ()
deleteWhere a1 = runQueryRep $ DeleteWhere a1

-- | The lifted version of 'Database.Persist.Sql.deleteWhereCount'
deleteWhereCount
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Int64
deleteWhereCount a1 = runQueryRep $ DeleteWhereCount a1

-- | The lifted version of 'Database.Persist.Sql.updateWhereCount'
updateWhereCount
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [Update record] -> m Int64
updateWhereCount a1 a2 = runQueryRep $ UpdateWhereCount a1 a2

#if !MIN_VERSION_persistent(2,13,0)
-- | The lifted version of 'Database.Persist.Sql.deleteCascade'
deleteCascade
  :: (DeleteCascade record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m ()
deleteCascade a1 = runQueryRep $ DeleteCascade a1
#endif

#if !MIN_VERSION_persistent(2,13,0)
-- | The lifted version of 'Database.Persist.Sql.deleteCascadeWhere'
deleteCascadeWhere
  :: (DeleteCascade record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m ()
deleteCascadeWhere a1 = runQueryRep $ DeleteCascadeWhere a1
#endif

-- | The lifted version of 'Database.Persist.Sql.parseMigration'
parseMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m (Either [Text] CautiousMigration)
parseMigration a1 = runQueryRep $ ParseMigration a1

-- | The lifted version of 'Database.Persist.Sql.parseMigration''
parseMigration'
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m CautiousMigration
parseMigration' a1 = runQueryRep $ ParseMigration' a1

-- | The lifted version of 'Database.Persist.Sql.printMigration'
printMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m ()
printMigration a1 = runQueryRep $ PrintMigration a1

-- | The lifted version of 'Database.Persist.Sql.showMigration'
showMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Text]
showMigration a1 = runQueryRep $ ShowMigration a1

-- | The lifted version of 'Database.Persist.Sql.getMigration'
getMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Sql]
getMigration a1 = runQueryRep $ GetMigration a1

-- | The lifted version of 'Database.Persist.Sql.runMigration'
runMigration
  :: (MonadSqlQuery m)
  => Migration -> m ()
runMigration a1 = runQueryRep $ RunMigration a1

#if MIN_VERSION_persistent(2,10,2)
-- | The lifted version of 'Database.Persist.Sql.runMigrationQuiet'
runMigrationQuiet
  :: (MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationQuiet a1 = runQueryRep $ RunMigrationQuiet a1
#endif

-- | The lifted version of 'Database.Persist.Sql.runMigrationSilent'
runMigrationSilent
  :: (MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationSilent a1 = runQueryRep $ RunMigrationSilent a1

-- | The lifted version of 'Database.Persist.Sql.runMigrationUnsafe'
runMigrationUnsafe
  :: (MonadSqlQuery m)
  => Migration -> m ()
runMigrationUnsafe a1 = runQueryRep $ RunMigrationUnsafe a1

#if MIN_VERSION_persistent(2,10,2)
-- | The lifted version of 'Database.Persist.Sql.runMigrationUnsafeQuiet'
runMigrationUnsafeQuiet
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationUnsafeQuiet a1 = runQueryRep $ RunMigrationUnsafeQuiet a1
#endif

-- | The lifted version of 'Database.Persist.Sql.getFieldName'
getFieldName
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => EntityField record typ -> m Text
getFieldName a1 = runQueryRep $ GetFieldName a1

-- | The lifted version of 'Database.Persist.Sql.getTableName'
getTableName
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m Text
getTableName a1 = runQueryRep $ GetTableName a1

-- | The lifted version of 'Database.Persist.Sql.withRawQuery'
withRawQuery
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> ConduitM [PersistValue] Void IO a -> m a
withRawQuery a1 a2 a3 = runQueryRep $ WithRawQuery a1 a2 a3

-- | The lifted version of 'Database.Persist.Sql.rawQueryRes'
rawQueryRes
  :: (MonadIO m2, MonadSqlQuery m)
  => Text -> [PersistValue] -> m (Acquire (ConduitM () [PersistValue] m2 ()))
rawQueryRes a1 a2 = runQueryRep $ RawQueryRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.rawQuery'
rawQuery
  :: (MonadResource m, MonadSqlQuery m)
  => Text -> [PersistValue] -> ConduitM () [PersistValue] m ()
rawQuery a1 a2 = fromAcquire $ runQueryRep $ RawQueryRes a1 a2

-- | The lifted version of 'Database.Persist.Sql.rawExecute'
rawExecute
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> m ()
rawExecute a1 a2 = runQueryRep $ RawExecute a1 a2

-- | The lifted version of 'Database.Persist.Sql.rawExecuteCount'
rawExecuteCount
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> m Int64
rawExecuteCount a1 a2 = runQueryRep $ RawExecuteCount a1 a2

-- | The lifted version of 'Database.Persist.Sql.rawSql'
rawSql
  :: (RawSql a, MonadSqlQuery m)
  => Text -> [PersistValue] -> m [a]
rawSql a1 a2 = runQueryRep $ RawSql a1 a2

-- | The lifted version of 'Database.Persist.Sql.transactionSave'
transactionSave
  :: (MonadSqlQuery m)
  => m ()
transactionSave = runQueryRep $ TransactionSave

#if MIN_VERSION_persistent(2,9,0)
-- | The lifted version of 'Database.Persist.Sql.transactionSaveWithIsolation'
transactionSaveWithIsolation
  :: (MonadSqlQuery m)
  => IsolationLevel -> m ()
transactionSaveWithIsolation a1 = runQueryRep $ TransactionSaveWithIsolation a1
#endif

-- | The lifted version of 'Database.Persist.Sql.transactionUndo'
transactionUndo
  :: (MonadSqlQuery m)
  => m ()
transactionUndo = runQueryRep $ TransactionUndo

#if MIN_VERSION_persistent(2,9,0)
-- | The lifted version of 'Database.Persist.Sql.transactionUndoWithIsolation'
transactionUndoWithIsolation
  :: (MonadSqlQuery m)
  => IsolationLevel -> m ()
transactionUndoWithIsolation a1 = runQueryRep $ TransactionUndoWithIsolation a1
#endif

{- Helpers -}

-- | A helper for functions that return a conduit.
fromAcquire :: MonadResource m => m (Acquire (ConduitM i o m a)) -> ConduitM i o m a
fromAcquire getAcquire = do
  (_, conduit) <- lift $ getAcquire >>= allocateAcquire
  conduit
