{-# LANGUAGE ConstraintKinds #-}
module Steroids.SchemaExtractor where
import           Text.InterpolatedString.Perl6 (qc)
import Data.String.Conv
import Safe
import Data.Maybe
import Database.PostgreSQL.Simple.FromRow (field, RowParser)
import qualified Steroids.Types as Types
import Steroids.Types (TableInfo(..), ColInfo(..))
import Steroids.Types (MonadConstraint)
import Database.PostgreSQL.Simple
import Control.Monad.Logger
import GHC.Stack
import Text.RE.TDFA.Text
import Control.Monad.IO.Class
import Data.List as DL
import Database.PostgreSQL.Simple.Types(PGArray(..))
import Data.Map.Strict as Map
import Control.Lens
import Debug.Trace

extractSchema
  :: MonadConstraint m
  => Connection
  -> Types.GlobalConfig
  -> m (Map.Map Types.TableName Types.TableInfo)
extractSchema conn cfg = do
  qresults <- fetchAllColumns conn cfg
  (pkConstraints, fkConstraints) <- fetchAllConstraints conn cfg
  pure $ generateTableInfoMap qresults pkConstraints fkConstraints


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

fetchAllConstraints
  :: (MonadLogger m, HasCallStack, MonadIO m)
  => Connection
  -> Types.GlobalConfig
  -> m ([Types.PKSide], [Types.FKResult])
fetchAllConstraints conn cfg = do
  let includedTables_ = reSource $ Types.cfgIncludeTables cfg
      excludedTables_ = reSource $ Types.cfgExcludeTables cfg
      schemas_ = reSource $ Types.cfgSchemas cfg
      queryParams = (schemas_, includedTables_, excludedTables_, includedTables_, excludedTables_)
  q <- liftIO (formatQuery conn fkQuery queryParams)
  logDebugNS "Steroids.Opaleye.SchemaExtractor" (toS q)
  rows <- liftIO $ query conn fkQuery queryParams
  pure $ (flip . flip DL.foldl') ([], []) rows $  \ (pks, fks) (conname, pkTable, pkKeys, mFkTable, mFkKeys) ->
    let pkSide = (pkTable, fromPGArray pkKeys)
    in case (mFkTable, mFkKeys) of
         (Nothing, Nothing) -> ( pkSide:pks, fks )
         (Just fkTable, Just fkKeys) ->
           let fkSide = ( conname
                        , pkSide
                        , (fkTable, fromPGArray fkKeys)
                        )
           in ( pks, fkSide:fks )
         x -> Prelude.error $ "Something is wrong. (mFkTable, mFkKeys) should either both be NULL, or NOT NULL. Found: " <> show x


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
INNER JOIN pg_catalog.pg_namespace pgn ON x.connamespace = pgn.oid
LEFT JOIN pg_catalog.pg_class fk_table ON x.confrelid=fk_table.oid
WHERE
  x.contype IN ('f', 'p')
  AND (pgn.nspname ~ ?)
  AND (pk_table.relname ~ ?)
  AND (NOT pk_table.relname ~ ?)
  AND (fk_table.relname IS NULL OR
    ((fk_table.relname ~ ?) AND (NOT fk_table.relname ~ ?))
  )
|])



generateTableInfoMap :: [Types.QResult] -> [Types.PKSide] -> [Types.FKResult] -> Map.Map Types.TableName Types.TableInfo
generateTableInfoMap qresults pks fks =
  let (qrIMap :: Map.Map Types.TableName [Types.QResult]) = Types.createMapBy (view _2) qresults
      (pkIMap :: Map.Map Types.TableName [Types.ColPosition]) = Map.fromList pks
      (fkIMap :: Map.Map Types.TableName [Types.FKResult]) = Types.createMapBy (view (_2._1)) fks
      (colIMap :: Map.Map Types.TableName (Map.Map Types.ColPosition Types.ColumnName)) =
        Map.map Map.fromList $
        Map.map (\qrs -> DL.map (\(_, _, cname, cpos, _, _, _, _) -> (cpos, cname)) qrs)  qrIMap

      getColName :: Types.TableName -> Types.ColPosition -> Types.ColumnName
      getColName tname colpos = fromJustNote ([qc|Could not find column at position {colpos}: {tname}|]) (Map.lookup colpos $ Map.findWithDefault (Map.empty) tname colIMap)



      -- we need to use zipWith & concatMap because of composite constraints,
      -- et. (open_slots, {1, 2}) => (trips, {5, 6}) which would result in TWO
      -- (or multiple) fk constraints.
      createFkConstraints :: Types.TableName -> [(Types.ColumnName, Types.TableName, Types.ColumnName)]
      createFkConstraints tname = DL.concatMap
                                 (\(_, (pkTable, pkPositions), (fkTable, fkPositions)) ->
                                    DL.zipWith (\pkPos fkPos ->
                                                  (getColName pkTable pkPos, fkTable, getColName fkTable fkPos)) pkPositions fkPositions)
                                 (Map.findWithDefault [] tname fkIMap)

      finalMap = (flip Map.mapWithKey) qrIMap $ \tname qrs ->
        let cmap = Map.fromList $ DL.map qresultToColInfo qrs
        in TableInfo
           { tableName = tname
           , pkInfo = maybe [] (DL.map (getColName tname)) (Map.lookup tname pkIMap)
           -- NOTE: We are keeping only those FK constraints that reference
           -- the PK of the foreign table. If we don't apply this restriction,
           -- then we will need to make a dependency graph of all tables and
           -- do a lot of code-gen while traversing that graph, in a very
           -- specific order.
           , fkConstraints = DL.filter (\(_, _, fkColName) -> fkColName == (Types.ColumnName "id")) (createFkConstraints tname)
           , columnMap = cmap
           }

  in finalMap
  where
    qresultToColInfo (_, _, cname, colpos, hasdef, nullable, ctype, cArrayType) =
      (cname, ColInfo
              {
                colName = cname
              , colRawPgType = fromMaybe ctype cArrayType
              , colDefault = hasdef
              , colNullable = nullable
              , colPosition = colpos
              , colArray = Types.ColIsArray (isJust cArrayType)
              , colArrayDim = if (isJust cArrayType) then 1 else 0
              })

--
-- Utils
--

getColInfoByColName :: Map.Map Types.TableName Types.TableInfo -> (Types.TableName, Types.ColumnName) -> Types.ColInfo
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
