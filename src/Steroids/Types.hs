module Steroids.Types where

import Data.Text as T
import Data.Map.Strict as Map
import Text.RE.TDFA.Text
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import Data.Aeson
import Data.String.Conv
import Cases (camelize)
import Data.List as DL
import Data.Word (Word32)
import Data.Aeson.TH
import Data.Text.Conversions
import Control.Monad.Logger (MonadLogger)
import GHC.Stack (HasCallStack)
import Control.Monad.IO.Class (MonadIO)


type QResult = (Oid, TableName, ColumnName, ColPosition, ColHasDefault, ColIsNullable, ColType, Maybe ColType)
type FKResult = (ConstraintName, PKSide, FKSide)
newtype ConstraintName = ConstraintName Text deriving (Eq, Show, FromField)
type PKSide = (TableName, [ColPosition])
type FKSide = (TableName, [ColPosition])
newtype SchemaName = SchemaName Text deriving (Eq, Show)
newtype TableName = TableName Text  deriving (Eq, Show, FromField, Ord, FromJSONKey, ToJSONKey, ToText)
newtype ColumnName = ColumnName Text deriving (Eq, Show, FromField, Ord, FromJSONKey, ToJSONKey, ToText)
newtype ColPosition = ColPosition Int deriving (Eq, Show, FromField, Ord)
newtype ColHasDefault = ColHasDefault Bool deriving (Eq, Show, FromField)
newtype ColType = ColType Text deriving (Eq, Show, Ord, FromField)
newtype FkOid = FkOid Oid deriving (Eq, Show)
newtype ColIsNullable = ColIsNullable Bool deriving (Eq, Show, FromField)
newtype RecordName = RecordName Text deriving (Eq, Show)
newtype FieldName = FieldName Text deriving (Eq, Show)
newtype FieldType = FieldType Text deriving (Eq, Show)
newtype ModuleName = ModuleName Text deriving (Eq, Show, ToText)
newtype PGWType = PGWType Text deriving (Eq, Show)
newtype PGRType = PGRType Text deriving (Eq, Show)
newtype HaskellType = HaskellType Text deriving (Eq, Show)
newtype RecordPrefix = RecordPrefix Text deriving (Eq, Show)
newtype ConstructorName = ConstructorName Text deriving (Eq, Show)
newtype ColIsArray = ColIsArray Bool deriving (Eq, Show, FromField)

$(deriveJSON defaultOptions ''ColumnName)
$(deriveJSON defaultOptions ''TableName)
$(deriveJSON defaultOptions ''RecordName)
$(deriveJSON defaultOptions ''FieldName)
$(deriveJSON defaultOptions ''ColHasDefault)
$(deriveJSON defaultOptions ''ColIsNullable)

data TableInfo = TableInfo
  { tableName :: !TableName
  , pkInfo :: !(Maybe ColumnName)
  , fkConstraints :: [(ColumnName, TableName, ColumnName)]
  , columnMap :: !(Map.Map ColumnName ColInfo)
  } deriving (Show)


data ColInfo = ColInfo
  { colName :: !ColumnName
  , colRawPGType :: !ColType
  , colDefault :: !ColHasDefault
  , colNullable :: !ColIsNullable
  , colArray :: !ColIsArray
  , colPosition :: !ColPosition
  , colArrayDim :: !Int
  } deriving (Show)

data FieldInfo = FieldInfo
  { fieldName :: !Text
  , fieldPGName :: !Text
  , fieldArrayDim :: !Int
  , fieldHaskellReadType :: !(ColIsNullable, QualifiedType)
  , fieldHaskellWriteType :: !(ColIsNullable, QualifiedType)
  , fieldPGReadType :: !(ColIsNullable, QualifiedType)
  , fieldPGWriteType :: !(ColIsNullable, ColHasDefault, QualifiedType)
  } deriving (Show)
-- $(makeLensesWith camelCaseFields ''FieldInfo)

data RecordInfo = RecordInfo
  { recordPolymorphicTypeName :: !Text
  , recordConstructurName :: !Text
  , recordFieldPrefix :: !Text
  , recordFields :: ![FieldInfo]
  , recordHaskellReadTypeName :: !Text
  , recordHaskellWriteTypeName :: !Text
  , recordPGReadTypeName :: !Text
  , recordPGWriteTypeName :: !Text
  , recordDeriving :: ![QualifiedType]
  , recordOpaleyeTableName :: !Text
  , recordPGTableName :: !Text
  , recordStrictFields :: Bool
  } deriving (Show)
-- $(makeLenses ''RecordInfo)


type ChecksumManifest = (Map.Map FilePath Word32)

data GlobalConfig = GlobalConfig
  { cfgSchemas :: RE
  , cfgIncludeTables :: RE
  , cfgExcludeTables :: RE
  , cfgRecordSettings :: TableInfo -> RecordInfo
  , cfgFieldSettings :: TableInfo -> ColInfo -> FieldInfo
  , cfgModuleSetings :: RecordInfo -> ModuleName
  , cfgHaskellTypeSettings :: ColInfo -> QualifiedType
  }

{-
data ConfigFile = ConfigFile
  { cfgGlobalConfig :: GlobalConfig
  , cfgTableConfig :: Map.Map TableName TableConfig
  , cfgI18nConfig :: Map.Map TableName I18NConfig
  , cfgI18nEscapeHatch :: Maybe (Map.Map TableName Text)
  } deriving (Generic)

data TableConfig = TableConfig
  { tcfgFields :: Map.Map ColumnName ColOverride
  , tcfgRecordName :: Maybe RecordName
  } deriving (Eq, Show, Generic)

data ColOverride = ColOverride
  { orideHaskellType :: Maybe (ModuleName, HaskellType)
  , oridePGWType :: Maybe (ModuleName, PGWType)
  , oridePGRType :: Maybe (ModuleName, PGRType)
  , orideDefault :: Maybe ColHasDefault
  , orideNullable :: Maybe ColIsNullable
  , orideFieldName :: Maybe FieldName
  , orideEnumName :: Maybe Text
  , orideEnumValues :: [(Text, Maybe Text)]
  , orideArrayDim :: Maybe Int
  } deriving (Eq, Show, Generic)
-}


type TypeWrapper x = (ModuleName, x) -> (ModuleName, x)

-- data ColInfo = ColInfo
--   {
--     colName :: ColumnName
--   , colRawPGType :: ColType
--   , colHaskellType :: (ModuleName, HaskellType)
--   , colPGWType :: (ModuleName, PGWType)
--   , colPGRType :: (ModuleName, PGRType)
--   , colDefault :: ColHasDefault
--   , colNullable :: ColIsNullable
--   , colArray :: ColIsArray
--   , colFieldName :: FieldName
--   , colPosition :: ColPosition
--   , colAutoEnum :: Bool
--   , colEnumValues :: [(Text, Text)]
--   , colArrayDim :: Int
--   } deriving (Show)

data I18NConfig = I18NConfig
  {
    trTranslationTableName :: TableName
  , trTranslationTablePGW :: Text
  , trBaseRecordName :: RecordName
  , trFields :: Map.Map ColumnName TranslationData
  } deriving (Show, Generic)

data TranslationData = NestedTrField NTransField | TrField TransField
  deriving (Show, Generic)

data NTransField = NTransField
 {
  tcfgPaths :: [PathHRN]
 } deriving (Show, Generic)
-- instance FromJSON NTransField

data PathHRN = PathHRN
  {
    trppathValue :: Text
  , trpHRN :: Text
  } deriving (Show, Generic)

-- instance FromJSON PathHRN where
--   parseJSON = genericParseJSON (aesonDrop 3 id){omitNothingFields=True}

newtype TransField = TransField
  {
    trHRN :: Text
  } deriving (Show, Generic)
instance FromJSON TransField


-- instance FromJSON ConfigFile where
--   parseJSON = genericParseJSON (aesonPrefix camelCase){omitNothingFields=True}

-- data TableInfo = TableInfo
--   {
--     tableName :: TableName
--   , pkInfo :: Maybe (ColumnName, (ModuleName, HaskellType))
--   , fkConstraints :: [(ColumnName, TableName, ColumnName)]
--   , columnMap :: Map.Map ColumnName ColInfo
--   , recordName :: RecordName
--   , polyConstructorName :: ConstructorName
--   , recordPrefix :: RecordPrefix
--   , exportList :: Set.Set ModuleName
--   , opaleyeTableName :: Text
--   , writeProtectedColumns :: [ColumnName]
--   } deriving (Show)


capitalizeFirstChar :: Text  -> Text
capitalizeFirstChar s = let (x, y) = T.splitAt 1 s
                        in T.append (T.toUpper x) y

pascalize :: (Data.String.Conv.StringConv a Text) => a -> Text
pascalize a = capitalizeFirstChar $ camelize $ toS a

createMapBy :: (Ord k) => (v -> k) -> [v] -> Map.Map k [v]
createMapBy keyFn as = Map.map DL.reverse $ DL.foldl' (\memo a -> Map.insertWith (++) (keyFn a) [a] memo) Map.empty as


qualifiedName :: QualifiedType -> Text
qualifiedName QualifiedType{..} = T.concat [qModuleName, ".", qTypeName]

pkParentModule :: Text
pkParentModule = "AutoGenerated.PrimaryKeys"

lensClassParentModule :: Text
lensClassParentModule = "AutoGenerated.Classes"


--
-- Code Snippet
--

data CodeSnippet = CodeSnippet
  { codeSnippet :: !Text
  , importList :: ![ImportItem]
  , newIdentifiers :: ![Text]
  } deriving (Show)

instance Semigroup CodeSnippet where
  (<>) x y = CodeSnippet
    { codeSnippet = T.concat [codeSnippet x, "\n", codeSnippet y]
    , importList = (importList x) <> (importList y)
    , newIdentifiers = (newIdentifiers x) <> (newIdentifiers y)
    }

instance Monoid CodeSnippet where
  mempty = CodeSnippet
    { codeSnippet = ""
    , importList = []
    , newIdentifiers = []
    }

data QualifiedType = QualifiedType
  { qModuleName :: !Text
  , qTypeName :: !Text
  }
  deriving (Eq, Show)

data ImportItem = ImportItem
  { importQualifiedType :: !QualifiedType
  , importConstructors :: Bool
  } deriving (Eq, Show)

withNoConstructors :: QualifiedType -> ImportItem
withNoConstructors qt = ImportItem qt False

withAllConstructors :: QualifiedType -> ImportItem
withAllConstructors qt = ImportItem qt True


type MonadConstraint m = (MonadLogger m, HasCallStack, MonadIO m)

class HasSettings m where
  getRecordSettings :: m (TableInfo -> RecordInfo)
  getFieldSettings :: m (TableInfo -> ColInfo -> FieldInfo)
  getModuleSettings :: m (RecordInfo -> ModuleName)
  getHaskellType :: m (ColType -> QualifiedType)
