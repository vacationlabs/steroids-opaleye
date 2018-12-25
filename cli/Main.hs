{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Steroids.Types as Types
import qualified Steroids.SchemaExtractor as Schema
import Steroids.RecordGenerator as Record
import Steroids.Control as Control
-- import Steroids.EnumGenerator as Enum
-- import Steroids.LensClassGenerator as Lens
import Steroids.PrimaryKeyGenerator as PK
-- import qualified Options.Applicative as Opts
-- import qualified Data.Text as T
-- import Data.Text (Text)
-- import System.FilePath (FilePath)
-- import qualified System.FilePath as FP
import qualified OptParser as Args
import OptParser (Args)
import qualified Options.Applicative as Opts
-- import Control.Monad (void)
import Database.PostgreSQL.Simple as PGS
import GHC.Stack (HasCallStack)
import Control.Monad.Reader
import qualified Control.Monad.Logger as MLogger
import qualified System.Log.FastLogger as FLogger
-- import Data.String.Conv (toS)
import qualified System.IO.Temp as Temp
import qualified Data.Map.Strict as Map
import qualified Data.List as DL
import Control.Lens
import Text.InterpolatedString.Perl6 (qc)
import Debug.Trace
-- import Safe (fromJustNote)

data ScriptEnv = ScriptEnv
  { fastLogger :: FLogger.FastLogger
  , tempDir :: FilePath
  , outputDir :: FilePath
  }

type ScriptM = ReaderT ScriptEnv IO

instance {-# OVERLAPS #-} MLogger.MonadLogger ScriptM where
  monadLoggerLog loc lsource llevel lstr = do
    flogger <- fastLogger <$> ask
    liftIO $ flogger $ MLogger.defaultLogStr loc lsource llevel (MLogger.toLogStr lstr)

main :: (HasCallStack) => IO ()
main = do
  args <- Opts.customExecParser Args.parserPrefs Args.parserInfo
  conn <- getConnection args
  FLogger.withFastLogger (FLogger.LogStdout FLogger.defaultBufSize) $ \flogger -> do
    Temp.withSystemTempDirectory "autogen" $ \tdir ->
      let cfg = getGlobalConfig args
          env = ScriptEnv
                { fastLogger = flogger
                , tempDir = tdir
                , outputDir = Args.outputDir args
                }
      in runReaderT (script conn cfg :: ScriptM ()) env

script :: PGS.Connection -> GlobalConfig -> ScriptM ()
script conn cfg = do
  -- TODO: any way to control record-setting via args OR config-file?
  tdir <- tempDir <$> ask
  outdir <- outputDir <$> ask

  -- Map.Map Types.TableName (Types.TableInfo, Types.RecordInfo, Types.ModuleName)
  tableMap <- (Control.applySettings cfg) <$>
              (Schema.extractSchema conn cfg)

  let tableTuples = Map.elems tableMap
      recordInfos = DL.map (^. _2) tableTuples

  let recordModules = DL.map (^. _3) tableTuples
  Control.ensureDirectories tdir recordModules
  modelChecksums <- foldM (generateModule tdir) Map.empty tableTuples

  let (pkNewtypes, pkPhantoms) = PK.extractPkSettings recordInfos
      pkModules = (DL.map (ModuleName . Types.qModuleName . Types.newtypeName) pkNewtypes) ++
                  (DL.map (ModuleName . Types.qModuleName . Types.phSynonym) pkPhantoms)
  when (not $ DL.null pkNewtypes)
    (Prelude.error $ "Only phantom-types support for primary keys")
  Control.ensureDirectories tdir pkModules
  pkChecksum <- foldM (generatePhantomPrimaryKey tdir) Map.empty pkPhantoms

  let checksumManifest = Map.unions [modelChecksums, pkChecksum]
  Control.copyNewFiles tdir outdir checksumManifest


getConnection :: (HasCallStack) => Args -> IO PGS.Connection
getConnection args = PGS.connect $ PGS.ConnectInfo
  { PGS.connectHost = Args.pgHost args
  , PGS.connectPort = fromIntegral $ Args.pgPort args
  , PGS.connectUser = Args.pgUser args
  , PGS.connectPassword = Args.pgPassword args
  , PGS.connectDatabase = Args.pgDatabase args
  }

getGlobalConfig :: Args -> Types.GlobalConfig
getGlobalConfig args =
  let customTypeMap = Map.fromList $ Args.typeMap args
      cfg = GlobalConfig
            { cfgSchemas = Args.includeSchema args
            , cfgIncludeTables = Args.includeTable args
            , cfgExcludeTables = Args.excludeTable args
            , cfgRecordSettings = Control.defaultRecordSettings cfg
            , cfgModuleSetings = Control.defaultModuleSettings cfg
            , cfgFieldSettings = Control.defaultFieldSettings cfg
            , cfgHaskellTypeSettings = Control.getHaskellType $ customTypeMap <> Control.defaultHaskellTypeMap
            , cfgPkSetting = case Args.newtypes args of
                Args.NTNone -> Control.defaultNoNewtypeSetting
                Args.NTPrimaryKeys -> Control.defaultPhantomNewtypeSetting cfg
                Args.NTBoth -> Prelude.error "PK + FK not implemented"
            }
  in cfg


generateModule
  :: (MonadConstraint m)
  => FilePath
  -> ChecksumManifest
  -> (Types.TableInfo, Types.RecordInfo, Types.ModuleName)
  -> m (ChecksumManifest)
generateModule tdir checksumManifest (_, rinfo@RecordInfo{..}, mname) = do
  (relativePath, checksum) <- Control.writeModule tdir mname contents
  pure $ Map.insert relativePath checksum checksumManifest
  where
    CodeSnippet{..} =
      (mempty { importList = [ withNoConstructors $ QualifiedType "Prelude" "String" ] })
      <> (Record.generateRecord rinfo)
      <> (Record.generateConcreteRecordType rinfo (withHaskellWrappers fieldHaskellReadType) recordHaskellReadTypeName)
      <> (Record.generateConcreteRecordType rinfo (withHaskellWrappers fieldHaskellWriteType) recordHaskellWriteTypeName)
      <> (Record.generateConcreteRecordType rinfo withPgReadWrappers recordPgReadTypeName)
      <> (Record.generateConcreteRecordType rinfo withPgWriteWrappers recordPgWriteTypeName)
      <> (Record.generateOpaleyeTable rinfo)
      <> (Record.generateProductProfunctor rinfo)
      -- <> (LGen.generateLenses rinfo)
    contents = ([qc|{Control.generateModuleHeader mname importList newIdentifiers}

\{-# ANN module ("HLint: ignore Avoid lambda" :: Prelude.String) #-}

{codeSnippet}
|])

generatePhantomPrimaryKey
  :: (MonadConstraint m)
  => FilePath
  -> ChecksumManifest
  -> PhantomTypeInfo
  -> m (ChecksumManifest)
generatePhantomPrimaryKey tdir checksumManifest phInfo = do
  (relativePath, checksum) <- Control.writeModule tdir mname contents
  pure $ Map.insert relativePath checksum checksumManifest
  where
    mname = ModuleName $ qModuleName $ phSynonym phInfo
    CodeSnippet{..} = PK.generatePhantom phInfo
    contents = ([qc|{Control.generateModuleHeader mname importList newIdentifiers}

{codeSnippet}
|])
