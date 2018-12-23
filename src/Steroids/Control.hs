module Steroids.Control where

import Steroids.Types as Types
import Steroids.SchemaExtractor as Schema
import Steroids.RecordGenerator as Rec
import Data.Text as T
import Data.List as DL
import Data.Map.Strict as Map
import Data.Set as Set
import Text.InterpolatedString.Perl6 (qc)
import GHC.Stack
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens as L
import Cases
import System.FilePath
import Data.Text.IO as TIO
import Control.Monad.Logger
import Data.Word (Word32)
import qualified Data.ByteString as BS
import Data.Digest.CRC32 (crc32)
import Data.String.Conv
import System.Directory 
import Text.Countable (singularize)
import System.IO as SIO


applyRecordSettings
  :: (HasCallStack)
  => Map.Map Types.TableName Schema.TableInfo
  -> (Schema.TableInfo -> Rec.RecordInfo)
  -> [Rec.RecordInfo]
applyRecordSettings tableMap recordSettingFn =
  DL.map recordSettingFn $ Map.elems tableMap

defaultOpaleyeRecordSettings
  :: (HasCallStack)
  => Schema.TableInfo
  -> Rec.RecordInfo
defaultOpaleyeRecordSettings tinfo@Schema.TableInfo{..} = Rec.RecordInfo
  { recordPolymorphicTypeName = T.append cname "Poly"
  , recordConstructurName = T.append cname "Poly"
  , recordFieldPrefix = T.append "_" (singularize $ T.toLower $ camelize tname)
  , recordFields =
      DL.map (defaultOpaleyFieldSettings tinfo) $
      DL.sortBy (\x y -> compare (Schema.colPosition x) (Schema.colPosition y)) $
      Map.elems columnMap
  , recordHaskellReadTypeName = cname
  , recordHaskellWriteTypeName = T.append cname "W"
  , recordPGReadTypeName = T.append cname "PGR"
  , recordPGWriteTypeName = T.append cname "PGW"
  , recordDeriving =
    [ QualifiedType "Prelude" "Eq"
    , QualifiedType "Prelude" "Show"
    ]
  , recordOpaleyeTableName = T.append "tableFor" (singularize $ pascalize tname)
  , recordPGTableName = tname
  , recordStrictFields = True
  }
  where
    cname = singularize $ pascalize tname
    (TableName tname) = tableName

defaultOpaleyFieldSettings
  :: (HasCallStack)
  => Schema.TableInfo
  -> Schema.ColInfo
  -> FieldInfo
defaultOpaleyFieldSettings tinfo@Schema.TableInfo{..} cinfo@Schema.ColInfo{..} = FieldInfo
  { fieldName = camelize cname
  , fieldPGName = cname
  , fieldArrayDim = case colArray of
      ColIsArray False -> 0
      ColIsArray True -> colArrayDim
  , fieldHaskellReadType = (colNullable, getDefaultHaskellType cinfo)
  , fieldHaskellWriteType = (colNullable, getDefaultHaskellType cinfo)
  , fieldPGReadType = (colNullable, getDefaultPGType cinfo)
  , fieldPGWriteType = case (colNullable, colDefault) of
      (ColIsNullable True, ColHasDefault True) -> (ColIsNullable True, ColHasDefault False, getDefaultPGType cinfo)
      _ -> (colNullable, colDefault, getDefaultPGType cinfo)
  }
  where
    (ColumnName cname) = colName


getDefaultHaskellType
  :: (HasCallStack)
  => Schema.ColInfo
  -> QualifiedType
getDefaultHaskellType cinfo@Schema.ColInfo{colRawPGType=(ColType ctype)} = case ctype of
  "jsonb" -> QualifiedType "Data.Aeson" "Value"
  "json" -> QualifiedType "Data.Aeson" "Value"
  "uuid" -> QualifiedType "Data.UUID" "UUID"
  "timestamptz" -> QualifiedType "Foundation.Types.Time" "ZonedTime"
  "timestamp" -> QualifiedType "Data.Time" "LocalTime"
  "time" -> QualifiedType "Data.Time" "TimeOfDay"
  "text" -> QualifiedType "Data.Text" "Text"
  "numeric" -> QualifiedType "Data.Decimal" "Decimal"
  "int2" -> QualifiedType "Data.Int" "Int16"
  "int4" -> QualifiedType "Prelude" "Integer"
  "int8" -> QualifiedType "Data.Int" "Int64"
  "float8" -> QualifiedType "Prelude" "Double"
  "float4" -> QualifiedType "Prelude" "Float"
  "date" -> QualifiedType "Data.Time" "Day"
  "bool" -> QualifiedType "Prelude" "Bool"
  "varchar" -> QualifiedType "Data.Text" "Text"
  "hstore" -> QualifiedType "Opaleye.TH" "HStoreMap"
  "inet" -> QualifiedType "Data.Text" "Text" -- TODO
  "bpchar" -> QualifiedType "Data.Text" "Text"
  t -> error ([qc|Unhandled PG Type found in {cinfo} ==> {t}|])

getDefaultPGType
  :: (HasCallStack)
  => Schema.ColInfo
  -> QualifiedType
getDefaultPGType cinfo@Schema.ColInfo{colRawPGType=(ColType ctype)} = case ctype of
  "varchar" -> QualifiedType "Opaleye" "PGText"
  "hstore" -> QualifiedType "Opaleye.TH" "PGHStore"
  y -> QualifiedType "Opaleye" (T.append "PG" (capitalizeFirstChar y))


ensureDirectories
  :: (HasCallStack, MonadIO m, MonadLogger m)
  => FilePath
  -> [ModuleName]
  -> m ()
ensureDirectories tdir modules =
  let uniqDirs = Set.fromList $ DL.map relativePathForModule modules
  in forM_ (Set.toList uniqDirs) $ \ m -> do
    let p = tdir ++ "/" ++ m
    liftIO $ createDirectoryIfMissing True p
    $(logDebug) ([qc|Created {p}|])

relativePathForModule :: ModuleName -> FilePath
relativePathForModule (ModuleName mname) =
  let pathSegments = T.splitOn "." mname
  in toS $ T.intercalate "/" (DL.init pathSegments)

writeModule
  :: (HasCallStack, MonadIO m, MonadLogger m)
  => FilePath
  -> ModuleName
  -> Text
  -> m (FilePath, Word32)
writeModule tdir m@(ModuleName mname) contents = do
  let pathSegments = T.splitOn "." mname
      fname = toS $ T.append (DL.last pathSegments) ".hs"
      relativePath = (relativePathForModule m) ++ "/" ++ fname
      fullpath = (tdir ++ "/" ++ relativePath)
  liftIO $ TIO.writeFile fullpath contents
  let c = crc32 (toS contents :: BS.ByteString)
  $(logDebug) ([qc|{fullpath} => {c}|])
  pure (relativePath, c)

copyNewFiles
  :: (HasCallStack, MonadIO m, MonadLogger m)
  => FilePath
  -> FilePath
  -> ChecksumManifest
  -> m ()
copyNewFiles tdir autogenDir newManifest = do
  -- figure out the checksum manifest of the existing directory
  existingManifest <- liftIO $ (doesDirectoryExist autogenDir) >>= \case
    False -> do
      SIO.putStrLn $ "[error] Target dir does not exist. Creating it: " ++ autogenDir
      removePathForcibly autogenDir
      createDirectory autogenDir
      pure Map.empty
    True -> generateExistingManifest autogenDir

  -- figuring out which files we need to copy and which we need to delete...
  let (changedOrNewFiles, removedFiles) =
        (
          Map.differenceWith (\x@crc1 crc2 -> if (crc1==crc2) then Nothing else Just crc1) newManifest existingManifest
        , Map.difference existingManifest newManifest
        )

  -- ensuring that all new sub-directories in target directory exist
  liftIO $ mapM_ (createDirectoryIfMissing True) $ DL.map (\x -> autogenDir ++ "/" ++ (fst $ splitFileName x)) $ Map.keys changedOrNewFiles

  -- copying new/modified files
  void $ (flip Map.traverseWithKey) changedOrNewFiles $ \ relativePath _ -> liftIO $ do
    copyFile (tdir ++ "/" ++ relativePath) (autogenDir ++ "/" ++ relativePath)
    setPermissions (autogenDir ++ "/" ++ relativePath) emptyPermissions{readable=True, writable=False, executable=False}
    SIO.putStrLn ("[modified/created] " ++ relativePath)

  -- deleting unnecessary files
  void $ forM (Map.keys removedFiles) $ \relativePath -> liftIO $ do
    removeFile (autogenDir ++ "/" ++ relativePath)
    SIO.putStrLn ("[deleted] " ++ relativePath)

  -- and we're done...
  pure ()
  where
    generateExistingManifest dir = do
      dirList <- listDirectory dir
      foldM
        (\memo fpath -> do
            let fullpath = dir ++ "/" ++ fpath
            (doesDirectoryExist fullpath) >>= \case
              False -> (BS.readFile fullpath) >>= (\c -> pure $ Map.insert (makeRelative autogenDir fullpath) (crc32 c) memo)
              True -> (generateExistingManifest fullpath) >>= (\x -> pure $ Map.union memo x)
        )
        Map.empty
        dirList
