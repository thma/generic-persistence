module Database.GP
  ( selectById,
    select,
    count,
    entitiesFromRows,
    sql,
    upsert,
    insert,
    insertMany,
    update,
    updateMany,
    delete,
    deleteById,
    deleteMany,
    deleteManyById,
    setupTable,
    defaultSqliteMapping,
    defaultPostgresMapping,
    Conn (..),
    connect,
    Database (..),
    TxHandling (..),
    ConnectionPool,
    createConnPool,
    withResource,
    Entity (..),
    GToRow,
    GFromRow,
    columnNameFor,
    maybeFieldTypeFor,
    TypeInfo (..),
    typeInfo,
    PersistenceException (..),
    WhereClauseExpr,
    Field,
    field,
    (&&.),
    (||.),
    (=.),
    (>.),
    (<.),
    (>=.),
    (<=.),
    (<>.),
    like,
    between,
    in',
    isNull,
    not',
    sqlFun,
    allEntries,
    byId,
    orderBy,
    SortOrder (..),
    limit,
    limitOffset,
    NonEmpty (..),
    SqlValue,
    fromSql,
    toSql,
    quickQuery,
    run,
    commit,
    rollback,
    withTransaction,
    runRaw,
    disconnect,
  )
where

-- We are just re-exporting from the GenericPersistence module.
import           Database.GP.GenericPersistence
import           Database.HDBC                  (IConnection (disconnect, runRaw),
                                                 SqlValue, commit, fromSql,
                                                 quickQuery, rollback, run,
                                                 toSql, withTransaction)
