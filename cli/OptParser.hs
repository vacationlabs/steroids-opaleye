module OptParser where

import Options.Applicative as Opts
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.RE.TDFA.Text as RE
import System.FilePath (FilePath)
import qualified System.FilePath as FP
import qualified Control.Monad.Catch as E
import qualified Data.Functor.Identity as I
import Options.Applicative.Types (ArgPolicy(..))

data Args = Args
  { pgUser :: !String
  , pgPassword :: !String
  , pgHost :: !String
  , pgPort :: !Int
  , pgDatabase :: !String
  , includeSchema :: !RE.RE
  , includeTable :: !RE.RE
  , excludeTable :: !RE.RE
  , outputDir :: !FilePath
  }

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnError = True
  , prefShowHelpOnEmpty = True
  , prefColumns = 120
  }

parserInfo :: ParserInfo Args
parserInfo = ParserInfo
  { infoParser = argParser
  , infoFullDesc = True
  , infoProgDesc = mempty
  , infoHeader = mempty
  , infoFooter = mempty
  , infoFailureCode = 1
  , infoPolicy = Intersperse
  }

argParser :: Parser Args
argParser = Args
  <$> pgUserParser
  <*> pgPasswordParser
  <*> pgHostParser
  <*> pgPortParser
  <*> pgDatabaseParser
  <*> includeSchemaParser
  <*> includeTableParser
  <*> excludeTableParser
  <*> outputDirParser
  where
    -- regexReadM :: (String -> Either String RE.RE) -> Opts.ReadM RE.RE
    regexReadM = eitherReader RE.compileRegex

    allRegex = I.runIdentity $ RE.compileRegex ".*"
    noRegex = I.runIdentity $ RE.compileRegex "^$"

    pgUserParser = strOption $
      long "pg-user" <>
      help "Postgres username to use while extracting schema information"

    pgPasswordParser = strOption $
      long "pg-password" <>
      help "Postgres password to use while extracting schema information"

    pgHostParser = strOption $
      long "pg-host" <>
      help "Postgres host to use while extracting schema information" <>
      value "localhost" <>
      showDefault

    pgPortParser = option auto $
      long "pg-port" <>
      help "Postgres port to use while extracting schema information" <>
      value 5432

    pgDatabaseParser = strOption $
      long "pg-database" <>
      help "Postgres database to use while extracting schema information"

    includeSchemaParser =
      let r = "^(public)$"
      in option regexReadM  $
         long "include-schema-regex" <>
         help "Which schemas to use for obtainining table information (please specify a regular expression)" <>
         value (I.runIdentity $ RE.compileRegex r) <>
         showDefaultWith (const r)

    includeTableParser = option regexReadM  $
      long "include-table-regex" <>
      help "Which tables to include for code generation (please specify a regular expression)" <>
      value allRegex <>
      showDefaultWith (const ".*")

    excludeTableParser = option regexReadM  $
      long "exclude-table-regex" <>
      help "Which tables to exclude for code generation (please specify a regular expression)" <>
      value noRegex <>
      showDefaultWith (const "empty string, i.e. nothing is excluded")

    outputDirParser = strOption $
      long "output-dir" <>
      help "All auto-generated modules will be placed in this directory. CAUTION: This directory _may_ be completely nuked by this script -- do not use a directory which has manually written code."
