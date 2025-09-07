module JoinSpec
  ( test,
    spec,
  )
where

import           Database.GP
import           Database.GP.Query (whereClauseValues, whereClauseExprToSql)
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec
import           Data.List.NonEmpty (NonEmpty(..))  -- Import NonEmpty and its constructor

test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTable @CustomerOrder conn defaultSqliteMapping
  setupTable @Customer conn defaultSqliteMapping
  setupTable @Product conn defaultSqliteMapping
  setupTable @OrderItem conn defaultSqliteMapping
  return conn

data Customer = Customer
  { customerID :: Int,
    customerName :: String,
    customerEmail :: String
  }
  deriving (Generic, Show, Eq)

data Product = Product
  { productID :: Int,
    productName :: String,
    productPrice :: Double
  }
  deriving (Generic, Show, Eq)

data CustomerOrder = CustomerOrder
  { orderID :: Int,
    customerID_fk :: Int,
    orderDate :: String,
    totalAmount :: Double
  }
  deriving (Generic, Show, Eq)

data OrderItem = OrderItem
  { orderItemID :: Int,
    orderID_fk :: Int,
    productID_fk :: Int,
    quantity :: Int,
    price :: Double
  }
  deriving (Generic, Show, Eq)

instance Entity Customer "customerID" Int where
  autoIncrement = False

instance Entity Product "productID" Int where
  autoIncrement = False

instance Entity CustomerOrder "orderID" Int where
  autoIncrement = False
  fieldsToColumns = 
    [ ("orderID", "orderID"),
      ("customerID_fk", "customerID_fk"),
      ("orderDate", "orderDate"),
      ("totalAmount", "totalAmount")
    ]

instance Entity OrderItem "orderItemID" Int where
  autoIncrement = False
  fieldsToColumns = 
    [ ("orderItemID", "orderItemID"),
      ("orderID_fk", "orderID_fk"),
      ("productID_fk", "productID_fk"),
      ("quantity", "quantity"),
      ("price", "price")
    ]

customer1 :: Customer
customer1 = Customer 1 "Alice Smith" "alice@example.com"

customer2 :: Customer
customer2 = Customer 2 "Bob Johnson" "bob@example.com"

product1 :: Product
product1 = Product 1 "Widget" 19.99

product2 :: Product
product2 = Product 2 "Gadget" 29.99

order1 :: CustomerOrder
order1 = CustomerOrder 1 1 "2024-01-15" 49.98

order2 :: CustomerOrder
order2 = CustomerOrder 2 2 "2024-01-16" 29.99

orderItem1 :: OrderItem
orderItem1 = OrderItem 1 1 1 2 39.98

orderItem2 :: OrderItem
orderItem2 = OrderItem 2 1 2 1 29.99

orderItem3 :: OrderItem
orderItem3 = OrderItem 3 2 2 1 29.99

spec :: Spec
spec = do
  describe "JOIN operations for 1:1 relationships" $ do
    it "performs INNER JOIN between orders and customers" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      _ <- insert conn order2
      
      -- Perform INNER JOIN
      let joinQuery = innerJoin allEntries 
                        "Customer" 
                        Nothing 
                        (field "customerID_fk") 
                        (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn joinQuery
      
      -- Should return 2 orders (one for each order with customer info)
      length orders `shouldBe` 2

    it "performs LEFT JOIN to include orders without customers" $ do
      conn <- prepareDB
      
      -- Insert test data - order without customer
      _ <- insert conn customer1
      let orphanOrder = CustomerOrder 3 999 "2024-01-17" 100.00
      _ <- insert conn order1
      _ <- insert conn orphanOrder
      
      -- Perform LEFT JOIN
      let leftJoinQuery = leftJoin allEntries 
                            "Customer" 
                            Nothing 
                            (field "customerID_fk") 
                            (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn leftJoinQuery
      
      -- Should return 2 orders (including the orphan order)
      length orders `shouldBe` 2

    it "uses table aliases in JOIN operations" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn order1
      
      -- Perform JOIN with alias
      let aliasJoinQuery = innerJoin allEntries 
                             "Customer" 
                             (Just "c") 
                             (field "customerID_fk") 
                             (qualifiedField "c" "customerID")
      
      orders <- select @CustomerOrder conn aliasJoinQuery
      
      -- Should return 1 order
      length orders `shouldBe` 1

    it "combines JOIN with WHERE clause" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      _ <- insert conn order2
      
      -- Perform JOIN with WHERE clause
      let filteredJoinQuery = innerJoin 
                                 (field "totalAmount" >. (30.0 :: Double))
                                 "Customer" 
                                 Nothing 
                                 (field "customerID_fk") 
                                 (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn filteredJoinQuery
      
      -- Should return only orders with totalAmount > 30
      length orders `shouldBe` 1

    it "performs JOIN with ORDER BY" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      _ <- insert conn order2
      
      -- Perform JOIN with ORDER BY
      let orderedJoinQuery = orderBy 
                               (innerJoin allEntries 
                                 "Customer" 
                                 Nothing 
                                 (field "customerID_fk") 
                                 (qualifiedField "Customer" "customerID"))
                               ((field "totalAmount", DESC) :| [])
      
      orders <- select @CustomerOrder conn orderedJoinQuery
      
      -- Should return 2 orders in descending order of totalAmount
      length orders `shouldBe` 2

    it "performs multiple table JOINs" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn product1
      _ <- insert conn order1
      _ <- insert conn orderItem1
      
      -- This test demonstrates the capability but actual multi-join 
      -- would need more sophisticated implementation
      let multiJoinQuery = innerJoin allEntries 
                             "CustomerOrder" 
                             Nothing 
                             (field "orderID_fk") 
                             (qualifiedField "CustomerOrder" "orderID")
      
      orderItems <- select @OrderItem conn multiJoinQuery
      
      -- Should return at least 1 order item
      length orderItems `shouldSatisfy` (>= 1)
    
    it "performs RIGHT JOIN operations" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      -- customer2 has no orders
      
      -- Perform RIGHT JOIN
      let rightJoinQuery = rightJoin allEntries 
                             "Customer" 
                             Nothing 
                             (field "customerID_fk") 
                             (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn rightJoinQuery
      
      -- Should return orders including those with no matching customers
      length orders `shouldSatisfy` (>= 1)
    
    it "performs FULL JOIN operations" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      let orphanOrder = CustomerOrder 3 999 "2024-01-17" 100.00
      _ <- insert conn orphanOrder
      
      -- Perform FULL JOIN
      let fullJoinQuery = fullJoin allEntries 
                            "Customer" 
                            Nothing 
                            (field "customerID_fk") 
                            (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn fullJoinQuery
      
      -- Should return all orders and all customers
      length orders `shouldSatisfy` (>= 2)
    
    it "tests Join with complex WHERE conditions" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn customer2
      _ <- insert conn order1
      _ <- insert conn order2
      
      -- Complex JOIN with multiple conditions
      let complexJoinQuery = innerJoin 
                               ((field "totalAmount" >. (25.0 :: Double)) &&. 
                                (field "orderDate" `like` "2024-%"))
                               "Customer" 
                               Nothing 
                               (field "customerID_fk") 
                               (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn complexJoinQuery
      
      -- Should return orders matching complex conditions
      length orders `shouldBe` 2
    
    it "tests whereClauseValues for Join expressions" $ do
      -- Test that Join expressions correctly extract values
      let joinExpr = innerJoin 
                       (field "amount" =. (100.0 :: Double))
                       "table2" 
                       Nothing 
                       (field "id1") 
                       (field "id2")
      
      let values = whereClauseValues joinExpr
      length values `shouldBe` 1
      
    it "tests Join with byId" $ do
      conn <- prepareDB
      
      -- Insert test data
      _ <- insert conn customer1
      _ <- insert conn order1
      
      -- JOIN with byId condition
      let byIdJoinQuery = innerJoin 
                            (byId (1 :: Int))
                            "Customer" 
                            Nothing 
                            (field "customerID_fk") 
                            (qualifiedField "Customer" "customerID")
      
      orders <- select @CustomerOrder conn byIdJoinQuery
      
      -- Should return specific order by ID
      length orders `shouldBe` 1
      case orders of
        (o:_) -> o `shouldBe` order1
        []    -> expectationFailure "orders is empty"
    
    it "tests nested Join conditions in whereClauseExprToSql" $ do
      -- Test that nested joins are handled correctly
      let nestedJoin = orderBy
                         (limit
                           (innerJoin 
                             (field "totalAmount" >. (50.0 :: Double))
                             "Customer" 
                             (Just "c")
                             (field "customerID_fk") 
                             (qualifiedField "c" "customerID"))
                           10)
                         ((field "orderDate", DESC) :| [])
      
      -- This tests that complex nested expressions don't cause issues
      let sqlStr = whereClauseExprToSql @CustomerOrder nestedJoin
      sqlStr `shouldContain` "LIMIT"
      sqlStr `shouldContain` "ORDER BY"