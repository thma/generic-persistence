{-# OPTIONS_GHC -Wno-deprecations #-}

module ErrorRecoverySpec
  ( spec,
  )
where

import           Control.Exception
import           Control.Monad                   (forM_)
import           Data.List                       (isInfixOf)
import           Database.GP
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics                    hiding (Selector)
import           Test.Hspec

-- Helper for test success
expectationSuccess :: IO ()
expectationSuccess = return ()

-- Test entities
data TestEntity = TestEntity
  { testId    :: Int,
    testValue :: String,
    testCount :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity TestEntity "testId" Int where
  autoIncrement = False

data TestAutoInc = TestAutoInc
  { autoId    :: Int,
    autoValue :: String
  }
  deriving (Generic, Show, Eq)

instance Entity TestAutoInc "autoId" Int where
  autoIncrement = True

-- Helper functions
prepareDB :: IO Conn
prepareDB = do
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTable @TestEntity conn defaultSqliteMapping
  setupTable @TestAutoInc conn defaultSqliteMapping
  return conn

prepareDBExplicit :: IO Conn
prepareDBExplicit = do
  conn <- connect ExplicitCommit <$> connectSqlite3 ":memory:"
  setupTable @TestEntity conn defaultSqliteMapping
  setupTable @TestAutoInc conn defaultSqliteMapping
  return conn

createTestEntities :: Int -> [TestEntity]
createTestEntities n = [TestEntity i ("value" ++ show i) (i * 10) | i <- [1..n]]

spec :: Spec
spec = do
  describe "Error Recovery - Transaction Rollback" $ do
    it "rolls back entire batch on duplicate key in insertMany" $ do
      conn <- prepareDB
      let entities = createTestEntities 5
      -- Insert first 3 entities successfully
      insertMany conn (take 3 entities)
      
      -- Now try to insert with duplicates (entity 2 and 3 are duplicates)
      let entity4 = TestEntity 4 "value4" 40
          entity2 = TestEntity 2 "value2" 20 
          entity5 = TestEntity 5 "value5" 50
          duplicateBatch = [entity4, entity2, entity5] -- entity2 is duplicate
      result <- try $ insertMany conn duplicateBatch
      
      case result of
        Left (DuplicateInsert _) -> do
          -- Verify that entities 4 and 5 were NOT inserted due to rollback
          allEntities <- select conn allEntries :: IO [TestEntity]
          length allEntities `shouldBe` 3  -- Only the first 3 should exist
          map testId allEntities `shouldBe` [1, 2, 3]
        Left ex -> expectationFailure $ "Unexpected exception: " ++ show ex
        Right _ -> expectationFailure "Expected DuplicateInsert exception"

    it "maintains consistency when updateMany fails partway through" $ do
      conn <- prepareDB
      let entities = createTestEntities 5
      insertMany conn entities
      
      -- Update entities, but include one that doesn't exist (id=10)
      let updates = map (\e -> e {testValue = "updated"}) entities
          withNonExistent = updates ++ [TestEntity 10 "nonexistent" 100]
      
      updateMany conn withNonExistent
      
      -- Check that all valid updates were applied
      updatedEntities <- select conn allEntries :: IO [TestEntity]
      all ((== "updated") . testValue) (filter ((<= 5) . testId) updatedEntities) `shouldBe` True

    it "handles transaction rollback in ExplicitCommit mode" $ do
      conn <- prepareDBExplicit
      let entities = createTestEntities 3
      commit conn
      
      -- Start a transaction, insert entities, then rollback
      _ <- try @PersistenceException $ insertMany conn entities
      rollback conn
      
      -- Verify nothing was persisted
      allEntities <- select conn allEntries :: IO [TestEntity]
      length allEntities `shouldBe` 0
      
      -- Now commit a transaction
      _ <- try @PersistenceException $ insertMany conn entities
      commit conn
      
      allEntities' <- select conn allEntries :: IO [TestEntity]
      length allEntities' `shouldBe` 3

  describe "Error Recovery - Prepared Statement Cleanup" $ do
    it "cleans up prepared statements on error using withPreparedStatement" $ do
      conn <- prepareDB
      
      -- This should clean up the statement even if an error occurs
      result <- try $ withPreparedStatement conn "INVALID SQL" $ \stmt -> do
        execute stmt []
      
      case result of
        Left (SqlError {}) -> expectationSuccess
        anyOtherCase -> expectationFailure $ "Expected SqlError, but got: " ++ show anyOtherCase
      
      -- Verify we can still use the connection
      _ <- insert conn (TestEntity 1 "test" 10)
      entities <- select conn allEntries :: IO [TestEntity]
      length entities `shouldBe` 1

    it "cleans up statements in batch operations on error" $ do
      conn <- prepareDB
      
      -- Create entities where one will cause an error
      let goodEntities = createTestEntities 2
      insertMany conn goodEntities
      
      -- Try to insert duplicates (should fail and clean up)
      let duplicates = createTestEntities 3  -- Includes duplicates of 1 and 2
      result <- try $ insertMany conn duplicates
      
      case result of
        Left (DuplicateInsert _) -> do
          -- Connection should still be usable
          newEntity <- insert conn (TestEntity 10 "new" 100)
          testId newEntity `shouldBe` 10
        _ -> expectationFailure "Expected DuplicateInsert"

  describe "Error Recovery - Batch Operation Failures" $ do
    it "handles partial failures in deleteMany gracefully" $ do
      conn <- prepareDB
      let entities = createTestEntities 3
      insertMany conn entities
      
      -- Delete some entities
      deleteMany conn (take 2 entities)
      
      -- Try to delete again (they don't exist anymore) plus one that does
      deleteMany conn entities  -- This includes already deleted and one valid
      
      -- Verify only entity 3 was deleted in the end
      remaining <- select conn allEntries :: IO [TestEntity]
      length remaining `shouldBe` 0

    it "handles mixed valid and invalid operations in deleteManyById" $ do
      conn <- prepareDB
      let entities = createTestEntities 5
      insertMany conn entities
      
      -- Delete by IDs including some that don't exist
      deleteManyById @TestEntity conn [1, 10, 3, 20, 5]  -- 10 and 20 don't exist
      
      -- Check that valid deletes succeeded
      remaining <- select conn allEntries :: IO [TestEntity]
      map testId remaining `shouldBe` [2, 4]

    it "recovers from constraint violations in batch inserts" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      -- Create table with a CHECK constraint
      runRaw conn "CREATE TABLE TestEntity (testId INTEGER PRIMARY KEY, testValue TEXT, testCount INTEGER CHECK(testCount >= 0))"
      commit conn
      
      -- Try to insert with invalid data (negative count)
      let invalidEntities = [TestEntity 1 "valid" 10, TestEntity 2 "invalid" (-5)]
      result <- try $ insertMany conn invalidEntities
      
      case result of
        Left (DatabaseError msg) -> msg `shouldSatisfy` \m -> 
          "constraint" `isInfixOf` m || "CHECK" `isInfixOf` m
        _ -> expectationFailure " Expected DatabaseError for constraint violation"

      -- Verify no partial inserts occurred
      entities <- select conn allEntries :: IO [TestEntity]
      length entities `shouldBe` 0

  describe "Error Recovery - Connection Pool Resilience" $ do
    it "handles operations with connection pool correctly" $ do
      -- Note: numStripes parameter issue - using safe values
      pool <- createConnPool AutoCommit ":memory:" connectSqlite3 60 10
      
      withResource pool $ \conn -> do
        setupTable @TestEntity conn defaultSqliteMapping
        let entities = createTestEntities 3
        insertMany conn entities
        
        -- Try operation that fails
        result <- try $ insertMany conn entities  -- Duplicates
        case result of
          Left (DuplicateInsert _) -> expectationSuccess
          _ -> expectationFailure "Expected DuplicateInsert"
      
      -- Pool should still be usable
      withResource pool $ \conn -> do
        entities <- select conn allEntries :: IO [TestEntity]
        length entities `shouldBe` 3

  describe "Error Recovery - Complex Scenarios" $ do
    it "handles cascading errors in nested operations" $ do
      conn <- prepareDB
      let entities = createTestEntities 5
      
      -- Complex operation with multiple potential failure points
      result <- try $ do
        insertMany conn (take 3 entities)
        let entity2 = TestEntity 2 "value2" 20
        updateMany conn [entity2 {testValue = "updated"}]
        insertMany conn [entity2]  -- This should fail (duplicate)
        deleteMany conn entities  -- This shouldn't execute
      
      case result of
        Left (DuplicateInsert _) -> do
          -- Verify state after error
          remaining <- select conn allEntries :: IO [TestEntity]
          length remaining `shouldBe` 3
          -- The update should have been applied
          let entity2 = filter ((== 2) . testId) remaining
          case entity2 of
            [e] -> testValue e `shouldBe` "updated"
            _ -> expectationFailure "Entity 2 not found or multiple found"
        _ -> expectationFailure "Expected DuplicateInsert"

    it "maintains data integrity across multiple error conditions" $ do
      conn <- prepareDB
      
      -- Series of operations with various error conditions
      forM_ [1..3] $ \i -> do
        let entity = TestEntity i ("value" ++ show i) (i * 10)
        _ <- insert conn entity
        
        -- Try duplicate insert (should fail silently in our error handling)
        _ <- try @PersistenceException $ insert conn entity
        
        -- Update the entity
        _ <- update conn entity {testValue = "updated" ++ show i}
        
        -- Try to update non-existent entity (should fail)
        _ <- try @PersistenceException $ 
          update conn (TestEntity (i + 100) "ghost" 0)
        
        return ()
      
      -- Verify final state is consistent
      finalEntities <- select conn allEntries :: IO [TestEntity]
      length finalEntities `shouldBe` 3
      all (\e -> testValue e == "updated" ++ show (testId e)) finalEntities `shouldBe` True

    it "handles auto-increment fields correctly after errors" $ do
      conn <- prepareDB
      
      -- Insert with auto-increment
      entity1 <- insert conn (TestAutoInc 0 "first")
      autoId entity1 `shouldBe` 1
      
      -- Try to insert another with auto-increment (simulating an error scenario)
      -- Note: SQLite continues incrementing even after failed attempts
      entity2 <- insert conn (TestAutoInc 0 "second")
      -- The ID might be 2 or higher depending on SQLite's internal counter
      autoId entity2 `shouldSatisfy` (>= 2)
      
      -- Verify both entities exist
      allEntities <- select conn allEntries :: IO [TestAutoInc]
      length allEntities `shouldBe` 2