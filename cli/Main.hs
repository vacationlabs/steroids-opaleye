module Main where

import Steroids.Types as Types
import Steroids.SchemaExtractor as Schema
import Steroids.RecordGenerator as Record
import Steroids.Control as Control
import Steroids.EnumGenerator as Enum
import Steroids.LensClassGenerator as Lens
import Steroids.PrimaryKeyGenerator as PK
import qualified Options.Applicative as Opts
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.RE.TDFA.Text as RE
import System.FilePath (FilePath)
import qualified System.FilePath as FP
import qualified OptParser as Args
import OptParser (Args)
import qualified Options.Applicative as Opts
import Control.Monad (void)
import Database.PostgreSQL.Simple as PGS
import GHC.Stack (HasCallStack (..))
import Control.Monad.Reader
import qualified Control.Monad.Logger as MLogger
import qualified System.Log.FastLogger as FLogger
import Data.String.Conv (toS)

data ScriptEnv = ScriptEnv
  { fastLogger :: FLogger.FastLogger
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
    let cfg = getGlobalConfig args
        env = ScriptEnv
              { fastLogger = flogger
              }
    runReaderT (script conn cfg :: ScriptM ()) env

script :: (MonadConstraint m) => PGS.Connection -> GlobalConfig -> m ()
script conn cfg = do
  schema <- Schema.extractSchema conn cfg
  MLogger.logDebugNS "steroids-opaleye" $ toS $ show schema

getConnection :: (HasCallStack) => Args -> IO PGS.Connection
getConnection args = PGS.connect $ PGS.ConnectInfo
  { PGS.connectHost = Args.pgHost args
  , PGS.connectPort = fromIntegral $ Args.pgPort args
  , PGS.connectUser = Args.pgUser args
  , PGS.connectPassword = Args.pgPassword args
  , PGS.connectDatabase = Args.pgDatabase args
  }

getGlobalConfig :: Args -> Types.GlobalConfig
getGlobalConfig args = GlobalConfig
  { cfgSchemas = Args.includeSchema args
  , cfgIncludeTables = Args.includeTable args
  , cfgExcludeTables = Args.excludeTable args
  }
