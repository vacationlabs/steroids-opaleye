{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Steroids.Control where

import Steroids.Types as Types
import Data.Text as T
import Data.List as DL
import Data.Map.Strict as Map
import Data.Set as Set
import Text.InterpolatedString.Perl6 (qc)
import GHC.Stack
import Control.Monad
import Control.Monad.IO.Class
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
import Safe (fromJustNote)
import Control.Lens
import Data.Text.Conversions (toText)

-- applyRecordSettings
--   :: (HasCallStack)
--   => (TableInfo -> Types.RecordInfo)
--   -> (RecordInfo -> ColInfo -> FieldInfo)
--   -> (RecordInfo -> ModuleName)
--   -> [TableInfo]
--   -> [Types.RecordInfo]
-- applyRecordSettings rsettings fsettings msettings tinfos  =
--   DL.map recordSettingFn tinfos

-- applyPkSettings
--   :: ( HasCallStack )
--   => GlobalConfig
--   -> [(Types.TableInfo, Types.RecordInfo, Types.ModuleName)]
--   -> [(Types.TableInfo, Types.RecordInfo, Types.ModuleName, [Types.PKSetting])]
-- applyPkSettings cfg tableTuples =
--   (flip DL.map) tableTuples $ \ (tinfo@TableInfo{pkInfo}, rinfo, mname) ->
--   let (pkSettings, newRecordFields) = DL.foldl' (applyPkSetting tinfo rinfo) ([], (recordFields rinfo)) pkInfo
--   in ( tinfo
--      , rinfo { recordFields = newRecordFields }
--      , mname
--      , pkSettings )

--   where
--     applyPkSetting :: TableInfo -> RecordInfo -> ([Types.PKSetting], [FieldInfo]) -> ColumnName -> ([Types.PKSetting], [FieldInfo])
--     applyPkSetting tinfo rinfo (pkSettings, finfos) pkColName =
--       let pkFieldIdx = fromJustNote ([qc|Unable to find column name {pkColName} in: {finfos}|]) $
--                        DL.findIndex ((== (toText pkColName)) . fieldPgName) finfos
--           pkFieldInfo = finfos DL.!! pkFieldIdx
--           pkColInfo = fromJustNote ([qc|Unable to find column name {pkColName} in: {tinfo}|]) $
--                       Map.lookup pkColName (columnMap tinfo)
--           (a, _:b) = DL.splitAt pkFieldIdx finfos
--           (pkSetting, newfinfo) = (cfgPkSetting cfg) (tinfo, pkColInfo, rinfo, pkFieldInfo)
--       in ( pkSetting:pkSettings
--          , a ++ (newfinfo:b) )

applySettings
  :: (HasCallStack)
  => GlobalConfig
  -> Map.Map TableName TableInfo
  -> Map.Map TableName (TableInfo, RecordInfo, ModuleName)
applySettings GlobalConfig{cfgRecordSettings, cfgModuleSetings} tinfos =
  (flip Map.map) tinfos $ \tinfo -> let rinfo = cfgRecordSettings tinfo
                                        mname = cfgModuleSetings rinfo
                                    in (tinfo, rinfo, mname)

defaultRecordSettings
  :: (HasCallStack)
  => GlobalConfig
  -> TableInfo
  -> Types.RecordInfo
defaultRecordSettings GlobalConfig{cfgFieldSettings} tinfo@TableInfo{..} = Types.RecordInfo
  { recordPolymorphicTypeName = T.append cname "Poly"
  , recordConstructurName = T.append cname "Poly"
  , recordFieldPrefix = T.append "_" (singularize $ T.toLower $ camelize tname)
  , recordFields =
      DL.map (cfgFieldSettings tinfo) $
      DL.sortBy (\x y -> compare (colPosition x) (colPosition y)) $
      Map.elems columnMap
  , recordHaskellReadTypeName = cname
  , recordHaskellWriteTypeName = T.append cname "W"
  , recordPgReadTypeName = T.append cname "PGR"
  , recordPgWriteTypeName = T.append cname "PGW"
  , recordDerivedClasses =
    [ QualifiedType "Prelude" "Eq"
    , QualifiedType "Prelude" "Show"
    ]
  , recordOpaleyeTableName = T.append "tableFor" (singularize $ pascalize tname)
  , recordPgTableName = tname
  , recordStrictFields = True
  }
  where
    cname = singularize $ pascalize tname
    (TableName tname) = tableName

defaultFieldSettings
  :: (HasCallStack)
  => GlobalConfig
  -> TableInfo
  -> ColInfo
  -> FieldInfo
defaultFieldSettings GlobalConfig{cfgHaskellTypeSettings, cfgPkSetting} tinfo@TableInfo{..} cinfo@ColInfo{..} =
  let finfo = FieldInfo
              { fieldName = camelize cname
              , fieldPgName = cname
              , fieldArrayDim = case colArray of
                  ColIsArray False -> 0
                  ColIsArray True -> colArrayDim
              , fieldHaskellReadType = (colNullable, cfgHaskellTypeSettings cinfo)
              , fieldHaskellWriteType = (colNullable, cfgHaskellTypeSettings cinfo)
              , fieldPgReadType = (colNullable, getDefaultPgType cinfo)
              , fieldPgWriteType = case (colNullable, colDefault) of
                  (ColIsNullable True, ColHasDefault True) -> (ColIsNullable True, ColHasDefault False, getDefaultPgType cinfo)
                  _ -> (colNullable, colDefault, getDefaultPgType cinfo)
              , fieldPkSetting = NoNewtype
              }
      finalfinfo = case DL.elem colName pkInfo of
                     False -> finfo
                     True -> cfgPkSetting tinfo cinfo finfo
  in finalfinfo
  where
    (ColumnName cname) = colName

defaultNoNewtypeSetting
  :: TableInfo
  -> ColInfo
  -> FieldInfo
  -> FieldInfo
defaultNoNewtypeSetting _ _ finfo = finfo

defaultPhantomNewtypeSetting
  :: GlobalConfig
  -> TableInfo
  -> ColInfo
  -> FieldInfo
  -> FieldInfo
defaultPhantomNewtypeSetting _ TableInfo{tableName} ColInfo{colName} finfo@FieldInfo{fieldHaskellReadType=(_, ftype)} = newfinfo
  where
    pkStg = PhantomNewtype $ PhantomTypeInfo
      { phPhantomType = QualifiedType "Foundation.Types.PrimaryKey" "PK"
      , phSynonym = psyn
      , phCoreType = ftype
      }
    newfinfo = finfo
               & haskellReadType._2 .~ psyn
               & haskellWriteType._2 .~ psyn
               & pgReadType._2 .~ psyn
               & pgWriteType._3 .~ psyn
               & pkSetting .~ pkStg
    psyn = QualifiedType mname pkName
    mname = ([qc|AutoGenerated.PrimaryKeys.{pkName}|])
    pkName = pascalize (singularize $ toText tableName) <>
             pascalize (toText colName)


defaultModuleSettings
  :: (HasCallStack)
  => GlobalConfig
  -> RecordInfo
  -> ModuleName
defaultModuleSettings _ rinfo = ModuleName ([qc|AutoGenerated.Models.{recordHaskellReadTypeName rinfo}|])

defaultHaskellTypeMap :: Map.Map ColType QualifiedType
defaultHaskellTypeMap = Map.fromList $
  DL.map (over _1 ColType) $
  [ ( "jsonb", QualifiedType "Data.Aeson" "Value" )
  , ( "json", QualifiedType "Data.Aeson" "Value" )
  , ( "uuid", QualifiedType "Data.UUID" "UUID" )
  , ( "timestamptz", QualifiedType "Foundation.Types.Time" "ZonedTime" )
  , ( "timestamp", QualifiedType "Data.Time" "LocalTime" )
  , ( "time", QualifiedType "Data.Time" "TimeOfDay" )
  , ( "text", QualifiedType "Data.Text" "Text" )
  , ( "numeric", QualifiedType "Data.Decimal" "Decimal" )
  , ( "int2", QualifiedType "Data.Int" "Int16" )
  , ( "int4", QualifiedType "Prelude" "Integer" )
  , ( "int8", QualifiedType "Data.Int" "Int64" )
  , ( "float8", QualifiedType "Prelude" "Double" )
  , ( "float4", QualifiedType "Prelude" "Float" )
  , ( "date", QualifiedType "Data.Time" "Day" )
  , ( "bool", QualifiedType "Prelude" "Bool" )
  , ( "varchar", QualifiedType "Data.Text" "Text" )
  , ( "hstore", QualifiedType "Opaleye.TH" "HStoreMap" )
  , ( "inet", QualifiedType "Data.Text" "Text" ) -- TODO
  , ( "bpchar", QualifiedType "Data.Text" "Text" )
  ]

getHaskellType
  :: (HasCallStack)
  => Map.Map ColType QualifiedType
  -> ColInfo
  -> QualifiedType
getHaskellType tyMap cinfo@ColInfo{colRawPgType} =
  fromJustNote ([qc|Unhandled PG Type found in {cinfo} ==> {colRawPgType}|]) $
  Map.lookup colRawPgType tyMap

getDefaultPgType
  :: (HasCallStack)
  => Types.ColInfo
  -> QualifiedType
getDefaultPgType Types.ColInfo{colRawPgType=(ColType ctype)} = case ctype of
  "varchar" -> QualifiedType "Opaleye" "PGText"
  "hstore" -> QualifiedType "Opaleye.TH" "PGHStore"
  y -> QualifiedType "Opaleye" (T.append "PG" (capitalizeFirstChar y))


ensureDirectories
  :: (MonadConstraint m)
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
  :: (MonadConstraint m)
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
  :: (MonadConstraint m)
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
          Map.differenceWith (\crc1 crc2 -> if (crc1==crc2) then Nothing else Just crc1) newManifest existingManifest
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



generateModuleHeader
  ::(HasCallStack)
  => ModuleName
  -> [ImportItem]
  -> [Text]
  -> Text
generateModuleHeader (ModuleName mname) ilist elist = ([qc|\{-# LANGUAGE TemplateHaskell, DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies, DeriveAnyClass #-}
\{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
\{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
module {moduleNameWithExportList} where
{T.unlines uniqueImportList :: Text}
|])
  where
    moduleNameWithExportList = case elist of
                                 [] -> mname
                                 items -> ([qc|{mname} ({T.intercalate ", " $ DL.nub items})|])
    importMap :: Map.Map Text (Map.Map Text Bool) =
      DL.foldl'
      (
        \memo ImportItem{..} ->
          let QualifiedType{..} = importQualifiedType
          in Map.insertWith
             (
               \_ constrMap2 ->
                 Map.insertWith (||) qTypeName importConstructors constrMap2
             )
             qModuleName
             (Map.singleton qTypeName importConstructors)
             memo
      )
      Map.empty
      ilist


    importItemWithConstructors constrMap =
      DL.map
      (
        \(tyName, includeConstructors) -> case includeConstructors of
          False -> tyName
          True -> ([qc|{tyName}(..)|])
      )
      (Map.toList constrMap)

    uniqueImportList :: [Text] =
      DL.map (\(mname_, constrMap) -> ([qc|import {mname_} ({T.intercalate ", " $ importItemWithConstructors constrMap})|])) $
      Map.toList importMap
