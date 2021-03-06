module OptParser where

import Options.Applicative as Opts
import qualified Data.Text as T
-- import Data.Text (Text)
import qualified Text.RE.TDFA.Text as RE
import System.FilePath (FilePath)
-- import qualified System.FilePath as FP
import qualified Data.Functor.Identity as I
import Options.Applicative.Types (ArgPolicy(..))
import qualified Steroids.Types as Types
  -- import qualified Data.Map.Strict as Map
import Data.String.Conv (toS)
-- import qualified Data.List as DL

data NewtypeSetting = NTNone
                    | NTPrimaryKeys
                    | NTBoth
                    deriving (Eq, Show)

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
  , typeMap :: ![(Types.ColType, Types.QualifiedType)]
  , newtypes :: !NewtypeSetting
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
  <*> typeMapParser
  <*> newtypeParser
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
      value 5432 <>
      showDefault

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

    typeMapParser = many $ option (eitherReader decodeTypeMap) $
      long "type-mapping" <>
      metavar "PG_TYPE,FULLY_QUALIFIED_HASKELL_TYPE" <>
      help "Modify the default DB<>Haskell type-mappings. Use this to either override an existing mapping, or add new mappings."

    newtypeParser = option (eitherReader decodeNewtypeSetting) $
      long "newtype-setting" <>
      metavar "none,pk,both" <>
      help "'pk' = generate newtypes for primary-keys (eg. newtype UserId = UserId Int). 'both' = use the newtypes for foreign key references." <>
      value NTNone <>
      showDefaultWith displayNewtypeSetting

decodeNewtypeSetting :: String -> Either String NewtypeSetting
decodeNewtypeSetting x = case x of
  "none" -> Right NTNone
  "pk" -> Right NTPrimaryKeys
  "both" -> Right NTBoth
  _  -> Left $ "Unknown newtype setting: " <> show x

displayNewtypeSetting :: NewtypeSetting -> String
displayNewtypeSetting x = case x of
  NTNone -> "none"
  NTPrimaryKeys -> "pk"
  NTBoth -> "both"

decodeTypeMap :: String -> Either String (Types.ColType, Types.QualifiedType)
decodeTypeMap x =
  let xt = toS x
  in case T.splitOn "," xt of
    [ctype, htype] -> case T.breakOnEnd "." htype of
      ("", _) -> Left $ "Please specify fully-qualified name for for the Haskelly type in: " <> x
      (a, b) -> Right ( Types.ColType ctype
                      , Types.QualifiedType (T.dropEnd 1  a) b ) -- remove the trailing '.'
    _ -> Left $ "Please specify type-mappings in the correct format: " <> x
