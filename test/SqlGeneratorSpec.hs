module SqlGeneratorSpec
  ( test,
    spec,
  )
where

import           Database.GP.Entity
import           Database.GP.SqlGenerator
import           GHC.Generics
import           Test.Hspec

-- Test entity with autoincrement
data User = User
  { userID :: Int,
    userName :: String,
    userEmail :: String,
    userAge :: Int,
    userActive :: Bool
  }
  deriving (Generic, Show, Eq)

instance Entity User "userID" Int where
  autoIncrement = True

-- Test entity without autoincrement
data Product = Product
  { productID :: Int,
    productName :: String,
    productPrice :: Double,
    productInStock :: Bool
  }
  deriving (Generic, Show, Eq)

instance Entity Product "productID" Int where
  autoIncrement = False

-- Test entity with custom column mappings
data Article = Article
  { articleID :: Int,
    articleTitle :: String,
    articleContent :: String,
    articleViews :: Integer,
    articleRating :: Double,
    articleFeatured :: Bool
  }
  deriving (Generic, Show, Eq)

instance Entity Article "articleID" Int where
  autoIncrement = True
  fieldsToColumns =
    [ ("articleID", "id"),
      ("articleTitle", "title"),
      ("articleContent", "content"),
      ("articleViews", "view_count"),
      ("articleRating", "rating"),
      ("articleFeatured", "is_featured")
    ]

-- Custom column type mapping for testing
customMapping :: ColumnTypeMapping
customMapping = \case
  "AUTOINCREMENT" -> "BIGSERIAL"
  "Int"           -> "BIGINT"
  "[Char]"        -> "VARCHAR(500)"
  "Double"        -> "DECIMAL(10,2)"
  "Bool"          -> "BOOLEAN"
  "Integer"       -> "BIGINT"
  _               -> "TEXT"

test :: IO ()
test = hspec spec

spec :: Spec
spec = do
  describe "SQL Generator Functions" $ do
    describe "insertReturningStmtFor" $ do
      it "generates INSERT RETURNING statement for entity with autoincrement" $ do
        let stmt = insertReturningStmtFor @User
        stmt `shouldBe` "INSERT INTO User (userName, userEmail, userAge, userActive) VALUES (?, ?, ?, ?) RETURNING userID, userName, userEmail, userAge, userActive;"
      
      it "generates INSERT RETURNING statement for entity without autoincrement" $ do
        let stmt = insertReturningStmtFor @Product
        stmt `shouldBe` "INSERT INTO Product (productID, productName, productPrice, productInStock) VALUES (?, ?, ?, ?) RETURNING productID, productName, productPrice, productInStock;"
      
      it "generates INSERT RETURNING statement with custom column mappings" $ do
        let stmt = insertReturningStmtFor @Article
        stmt `shouldBe` "INSERT INTO Article (title, content, view_count, rating, is_featured) VALUES (?, ?, ?, ?, ?) RETURNING id, title, content, view_count, rating, is_featured;"

    describe "upsertStmtFor" $ do
      it "generates UPSERT statement for entity with autoincrement" $ do
        let stmt = upsertStmtFor @User
        stmt `shouldBe` "INSERT INTO User (userID, userName, userEmail, userAge, userActive) VALUES (?, ?, ?, ?, ?) ON CONFLICT (userID) DO UPDATE SET userID = ?, userName = ?, userEmail = ?, userAge = ?, userActive = ?;"
      
      it "generates UPSERT statement for entity without autoincrement" $ do
        let stmt = upsertStmtFor @Product
        stmt `shouldBe` "INSERT INTO Product (productID, productName, productPrice, productInStock) VALUES (?, ?, ?, ?) ON CONFLICT (productID) DO UPDATE SET productID = ?, productName = ?, productPrice = ?, productInStock = ?;"
      
      it "generates UPSERT statement with custom column mappings" $ do
        let stmt = upsertStmtFor @Article
        stmt `shouldBe` "INSERT INTO Article (id, title, content, view_count, rating, is_featured) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (id) DO UPDATE SET id = ?, title = ?, content = ?, view_count = ?, rating = ?, is_featured = ?;"

    describe "dropTableStmtFor" $ do
      it "generates DROP TABLE statement for regular entity" $ do
        let stmt = dropTableStmtFor @User
        stmt `shouldBe` "DROP TABLE IF EXISTS User;"
      
      it "generates DROP TABLE statement for entity with custom table name" $ do
        let stmt = dropTableStmtFor @Product
        stmt `shouldBe` "DROP TABLE IF EXISTS Product;"
      
      it "generates DROP TABLE statement for entity with custom column mappings" $ do
        let stmt = dropTableStmtFor @Article
        stmt `shouldBe` "DROP TABLE IF EXISTS Article;"

    describe "columnTypeFor" $ do
      it "maps types correctly with SQLite mapping" $ do
        columnTypeFor @User defaultSqliteMapping "userID" `shouldBe` "INTEGER"
        columnTypeFor @User defaultSqliteMapping "userName" `shouldBe` "TEXT"
        columnTypeFor @User defaultSqliteMapping "userAge" `shouldBe` "INTEGER"
        columnTypeFor @User defaultSqliteMapping "userActive" `shouldBe` "INT"
        columnTypeFor @Product defaultSqliteMapping "productPrice" `shouldBe` "REAL"
      
      it "maps types correctly with PostgreSQL mapping" $ do
        columnTypeFor @User defaultPostgresMapping "userID" `shouldBe` "serial"
        columnTypeFor @User defaultPostgresMapping "userName" `shouldBe` "varchar"
        columnTypeFor @User defaultPostgresMapping "userAge" `shouldBe` "numeric"
        columnTypeFor @User defaultPostgresMapping "userActive" `shouldBe` "boolean"
        columnTypeFor @Product defaultPostgresMapping "productPrice" `shouldBe` "numeric"
      
      it "handles custom column type mappings" $ do
        columnTypeFor @User customMapping "userID" `shouldBe` "BIGSERIAL"
        columnTypeFor @User customMapping "userName" `shouldBe` "VARCHAR(500)"
        columnTypeFor @User customMapping "userAge" `shouldBe` "BIGINT"
        columnTypeFor @User customMapping "userActive" `shouldBe` "BOOLEAN"
        columnTypeFor @Product customMapping "productPrice" `shouldBe` "DECIMAL(10,2)"
        columnTypeFor @Article customMapping "articleRating" `shouldBe` "DECIMAL(10,2)"
        columnTypeFor @Article customMapping "articleViews" `shouldBe` "BIGINT"
      
      it "handles unknown types with fallback" $ do
        let fallbackMapping = \case
              "AUTOINCREMENT" -> "SERIAL"
              "UnknownType"   -> "CUSTOM_TYPE"
              _               -> "DEFAULT_TYPE"
        columnTypeFor @User fallbackMapping "userName" `shouldBe` "DEFAULT_TYPE"

    describe "createTableStmtFor" $ do
      it "generates CREATE TABLE with SQLite types" $ do
        let stmt = createTableStmtFor @User defaultSqliteMapping
        stmt `shouldContain` "CREATE TABLE User"
        stmt `shouldContain` "userID INTEGER PRIMARY KEY"
        stmt `shouldContain` "userName TEXT"
        stmt `shouldContain` "userAge INTEGER"
        stmt `shouldContain` "userActive INT"
      
      it "generates CREATE TABLE with PostgreSQL types" $ do
        let stmt = createTableStmtFor @Product defaultPostgresMapping
        stmt `shouldContain` "CREATE TABLE Product"
        stmt `shouldContain` "productID numeric PRIMARY KEY"
        stmt `shouldContain` "productName varchar"
        stmt `shouldContain` "productPrice numeric"
        stmt `shouldContain` "productInStock boolean"
      
      it "generates CREATE TABLE with custom column mappings" $ do
        let stmt = createTableStmtFor @Article customMapping
        stmt `shouldContain` "CREATE TABLE Article"
        stmt `shouldContain` "id BIGSERIAL PRIMARY KEY"
        stmt `shouldContain` "title VARCHAR(500)"
        stmt `shouldContain` "view_count BIGINT"
        stmt `shouldContain` "rating DECIMAL(10,2)"
        stmt `shouldContain` "is_featured BOOLEAN"

    describe "insertStmtFor" $ do
      it "excludes autoincrement field from INSERT" $ do
        let stmt = insertStmtFor @User
        stmt `shouldBe` "INSERT INTO User (userName, userEmail, userAge, userActive) VALUES (?, ?, ?, ?);"
        stmt `shouldNotContain` "userID"
      
      it "includes all fields when no autoincrement" $ do
        let stmt = insertStmtFor @Product
        stmt `shouldBe` "INSERT INTO Product (productID, productName, productPrice, productInStock) VALUES (?, ?, ?, ?);"

    describe "updateStmtFor" $ do
      it "generates UPDATE statement with all fields" $ do
        let stmt = updateStmtFor @User
        stmt `shouldBe` "UPDATE User SET userID = ?, userName = ?, userEmail = ?, userAge = ?, userActive = ? WHERE userID = ?;"
      
      it "uses custom column names in UPDATE" $ do
        let stmt = updateStmtFor @Article
        stmt `shouldBe` "UPDATE Article SET id = ?, title = ?, content = ?, view_count = ?, rating = ?, is_featured = ? WHERE id = ?;"

    describe "selectFromStmt" $ do
      it "generates SELECT with WHERE clause" $ do
        let stmt = selectFromStmt @User (field "userName" =. "Alice")
        stmt `shouldContain` "SELECT userID, userName, userEmail, userAge, userActive FROM User"
        stmt `shouldContain` "WHERE userName = ?"
      
      it "handles complex WHERE conditions" $ do
        let stmt = selectFromStmt @Product ((field "productPrice" >. (10.0 :: Double)) &&. (field "productInStock" =. True))
        stmt `shouldContain` "SELECT productID, productName, productPrice, productInStock FROM Product"
        stmt `shouldContain` "WHERE"
        stmt `shouldContain` "AND"

    describe "countStmtFor" $ do
      it "generates COUNT statement with WHERE clause" $ do
        let stmt = countStmtFor @User allEntries
        stmt `shouldBe` "SELECT COUNT(*) FROM User WHERE 1=1;"
      
      it "generates COUNT with specific conditions" $ do
        let stmt = countStmtFor @Product (field "productInStock" =. True)
        stmt `shouldContain` "SELECT COUNT(*) FROM Product"
        stmt `shouldContain` "WHERE productInStock = ?"

    describe "deleteStmtFor" $ do
      it "generates DELETE statement for entity" $ do
        let stmt = deleteStmtFor @User
        stmt `shouldBe` "DELETE FROM User WHERE userID = ?;"
      
      it "uses custom column name in DELETE" $ do
        let stmt = deleteStmtFor @Article
        stmt `shouldBe` "DELETE FROM Article WHERE id = ?;"

    describe "selectFromStmtWithJoins" $ do
      it "generates SELECT with INNER JOIN" $ do
        let joinQuery = innerJoin allEntries "orders" Nothing (field "userID") (qualifiedField "orders" "user_id")
        let stmt = selectFromStmt @User joinQuery
        stmt `shouldContain` "SELECT User.userID, User.userName"
        stmt `shouldContain` "INNER JOIN orders"
        stmt `shouldContain` "ON userID = orders.user_id"
      
      it "generates SELECT with LEFT JOIN and alias" $ do
        let joinQuery = leftJoin allEntries "orders" (Just "o") (field "userID") (qualifiedField "o" "user_id")
        let stmt = selectFromStmt @User joinQuery
        stmt `shouldContain` "LEFT JOIN orders AS o"
        stmt `shouldContain` "o.*"
      
      it "handles multiple JOIN types" $ do
        let rightJoinQuery = rightJoin allEntries "products" Nothing (field "productID") (qualifiedField "products" "id")
        let stmt = selectFromStmt @Product rightJoinQuery
        stmt `shouldContain` "RIGHT JOIN products"
        
        let fullJoinQuery = fullJoin allEntries "categories" (Just "c") (field "productID") (qualifiedField "c" "product_id")
        let stmt2 = selectFromStmt @Product fullJoinQuery
        stmt2 `shouldContain` "FULL OUTER JOIN categories AS c"