{-# LANGUAGE ConstraintKinds #-}
module Steroids.SchemaExtractor where
import           Text.InterpolatedString.Perl6 (qc)
import Data.String.Conv
import Safe
import Data.Maybe
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (field, fieldWith, RowParser)
import qualified Steroids.Types as Types
import Database.PostgreSQL.Simple
import Control.Monad.Logger
import GHC.Stack
import Text.RE.TDFA.Text
import Control.Monad.IO.Class
import Data.List as DL
import Database.PostgreSQL.Simple.Types(PGArray(..))
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Text as T
import Control.Lens

type MonadConstraint m = (MonadLogger m, HasCallStack, MonadIO m)

data TableInfo = TableInfo
  { tableName :: Types.TableName
  , pkInfo :: Maybe Types.ColumnName
  , fkConstraints :: [(Types.ColumnName, Types.TableName, Types.ColumnName)]
  , columnMap :: Map.Map Types.ColumnName ColInfo
  } deriving (Show)

data ColInfo = ColInfo
  { colName :: Types.ColumnName
  , colRawPGType :: Types.ColType
  , colDefault :: Types.ColHasDefault
  , colNullable :: Types.ColIsNullable
  , colArray :: Types.ColIsArray
  , colPosition :: Types.ColPosition
  , colArrayDim :: Int
  } deriving (Show)


extractSchema
  :: MonadConstraint m
  => Connection
  -> Types.GlobalConfig
  -> m (Map.Map Types.TableName TableInfo)
extractSchema conn cfg = do
  qresults <- fetchAllColumns conn cfg
  fkConstraints <- fetchAllFkConstraints conn cfg
  pure $ generateTableInfoMap qresults fkConstraints


fetchAllColumns
  :: MonadConstraint m
  => Connection
  -> Types.GlobalConfig
  -> m [Types.QResult]
fetchAllColumns conn cfg = do
  let includeTables_ = reSource $ Types.cfgIncludeTables cfg
      excludedTables_ = reSource $ Types.cfgExcludeTables cfg
      schemas_ = reSource $ Types.cfgSchemas cfg
      queryParams = (schemas_, includeTables_, excludedTables_)
  q <- liftIO (formatQuery conn colQuery queryParams)
  $(logDebug) (toS q)
  liftIO $ queryWith colQueryParser conn colQuery queryParams

colQueryParser :: RowParser Types.QResult
colQueryParser = (,,,,,,,)
  <$> field -- oid
  <*> field -- table name
  <*> field -- column name
  <*> field -- col position
  <*> field -- col default
  <*> field -- col nullable
  <*> field -- col type
  <*> field -- col array type

colQuery :: Query
colQuery = ([qc|
SELECT
  (t.tablename :: regclass :: oid),
  t.tablename,
  c.attname,
  c.attnum,
  c.atthasdef,
  (not c.attnotnull),
  typ1.typname as basetye,
  typ2.typname as arraytype
FROM pg_catalog.pg_tables t
INNER JOIN pg_catalog.pg_attribute c ON c.attrelid=(t.tablename :: regclass :: oid)
INNER JOIN pg_catalog.pg_type typ1 on c.atttypid=typ1.oid
LEFT JOIN pg_catalog.pg_type typ2 ON typ1.typelem=typ2.oid
WHERE
  (t.schemaname ~ ?)
  AND c.attisdropped=false
  AND c.attnum>=1
  AND (t.tablename ~ ?)
  AND NOT (t.tablename ~ ?)
ORDER BY t.tablename, c.attnum
|])

fetchAllFkConstraints
  :: (MonadLogger m, HasCallStack, MonadIO m)
  => Connection
  -> Types.GlobalConfig
  -> m [Types.FKResult]
fetchAllFkConstraints conn cfg = do
  let includedTables_ = reSource $ Types.cfgIncludeTables cfg
      excludedTables_ = reSource $ Types.cfgExcludeTables cfg
      schemas_ = reSource $ Types.cfgSchemas cfg
      queryParams = (schemas_, includedTables_, excludedTables_, includedTables_, excludedTables_)
  q <- liftIO (formatQuery conn fkQuery queryParams)
  logDebugNS "Steroids.Opaleye.SchemaExtractor" (toS q)
  rows <- liftIO $ query conn fkQuery queryParams
  pure $ DL.map (\(conname, pkTable, pkKeys, fkTable, fkKeys) -> (conname, (pkTable, fromPGArray pkKeys), (fkTable, fromPGArray fkKeys))) rows


-- TODO: Apply schema filter
fkQuery :: Query
fkQuery = ([qc|
SELECT
  x.conname,
  pk_table.relname,
  x.conkey,
  fk_table.relname,
  x.confkey
FROM pg_catalog.pg_constraint x
INNER JOIN pg_catalog.pg_class pk_table ON x.conrelid=pk_table.oid
INNER JOIN pg_catalog.pg_class fk_table ON x.confrelid=fk_table.oid
INNER JOIN pg_catalog.pg_namespace pgn ON x.connamespace = pgn.oid
WHERE
  x.contype='f'
  AND (pgn.nspname ~ ?)
  AND (pk_table.relname ~ ?)
  AND (NOT pk_table.relname ~ ?)
  AND (fk_table.relname ~ ?)
  AND (NOT fk_table.relname ~ ?)
|])



generateTableInfoMap :: [Types.QResult] -> [Types.FKResult] -> Map.Map Types.TableName TableInfo
generateTableInfoMap qresults fks =
  let (qrIMap :: Map.Map Types.TableName [Types.QResult]) = Types.createMapBy (view _2) qresults
      (fkIMap :: Map.Map Types.TableName [Types.FKResult]) = Types.createMapBy (view (_2._1)) fks
      (colIMap :: Map.Map Types.TableName (Map.Map Types.ColPosition Types.ColumnName)) =
        Map.map Map.fromList $
        Map.map (\qrs -> DL.map (\(_, _, cname, cpos, _, _, _, _) -> (cpos, cname)) qrs)  qrIMap

      getColName :: (Types.TableName, Types.ColPosition) -> Types.ColumnName
      getColName arg@(tname, colpos) = fromJustNote ([qc|Could not find column for {arg}|]) (Map.lookup colpos $ Map.findWithDefault (Map.empty) tname colIMap)



      -- we need to use zipWith & concatMap because of composite constraints,
      -- et. (open_slots, {1, 2}) => (trips, {5, 6}) which would result in TWO
      -- (or multiple) fk constraints.
      createFkConstraints :: Types.TableName -> [(Types.ColumnName, Types.TableName, Types.ColumnName)]
      createFkConstraints tname = DL.concatMap
                                 (\(_, (pkTable, pkPositions), (fkTable, fkPositions)) ->
                                    DL.zipWith (\pkPos fkPos ->
                                                  (getColName (pkTable, pkPos), fkTable, getColName (fkTable, fkPos))) pkPositions fkPositions)
                                 (Map.findWithDefault [] tname fkIMap)

      finalMap = (flip Map.mapWithKey) qrIMap $ \tname qrs ->
        TableInfo
        { tableName = tname
        -- TODO: we should be looking at the col metadata to figure out whether it's a PK?
        , pkInfo = fmap (view _3) (DL.find (\qr -> qr ^. _3 == (Types.ColumnName "id")) qrs)
        -- NOTE: We are keeping only those FK constraints that reference
        -- the PK of the foreign table. If we don't apply this restriction,
        -- then we will need to make a dependency graph of all tables and
        -- do a lot of code-gen while traversing that graph, in a very
        -- specific order.
        , fkConstraints = DL.filter (\(_, _, fkColName) -> fkColName == (Types.ColumnName "id")) (createFkConstraints tname)
        , columnMap = (Map.fromList $ DL.map qresultToColInfo qrs)
        }

  in finalMap
  where
    qresultToColInfo (_, _, cname, colpos, hasdef, nullable, ctype, cArrayType) =
      (cname, ColInfo
              {
                colName = cname
              , colRawPGType = fromMaybe ctype cArrayType
              , colDefault = hasdef
              , colNullable = nullable
              , colPosition = colpos
              , colArray = Types.ColIsArray (isJust cArrayType)
              , colArrayDim = if (isJust cArrayType) then 1 else 0
              })

--
-- Utils
--

getColInfoByColName :: Map.Map Types.TableName TableInfo -> (Types.TableName, Types.ColumnName) -> ColInfo
getColInfoByColName tableMap args@(tname, cname) =
  let tinfo = fromJustNote ([qc|Couldn't find {tname} while looking up {args}|]) (Map.lookup tname tableMap)
  in fromJustNote ([qc|Couldn't find {cname} while looking up {args}|]) $ DL.find (\cinfo -> (colName cinfo) == cname) (columnMap tinfo)


{-

import Text.RE.TDFA.Text
import Database.PostgreSQL.Simple
import Control.Monad.Logger
import Data.Map.Strict as Map
import Data.Functor.Identity
import Steroids.Types
conn <- connectPostgreSQL "dbname=b2b host=localhost user=b2b password=b2b"
let star = runIdentity $ compileRegex ".*"
let gc = GlobalConfig (runIdentity $ compileRegex "(b2b|public)") Map.empty star (runIdentity $ compileRegex "(package_pricings)") []
runStdoutLoggingT $ extractSchema conn gc

-}
