{-# LANGUAGE RecordWildCards #-}
module Steroids.RecordGenerator where

import Steroids.Types
import Data.Text as T
import Data.List as DL
import Data.Map.Strict as Map
import Text.InterpolatedString.Perl6 (qc)
import GHC.Stack

{-
generateRecords :: (HasCallStack) => Map.Map TableName TableInfo -> ScriptM [ModuleName]
generateRecords tableMap = fmap Map.elems $ Control.Monad.forM tableMap $ \tinfo -> do
  let rname = recordName tinfo
      typeModules = DL.nub $ DL.map fst $
                    (Map.elems $ Map.map colHaskellType (columnMap tinfo))
                    ++ (Map.elems $ Map.map colPGWType (columnMap tinfo))
                    ++ (Map.elems $ Map.map colPGRType (columnMap tinfo))
      qualClassNames = Map.elems $ (flip Map.map) (columnMap tinfo) (\cinfo -> moduleNameForLens $ fieldToClassName (colFieldName cinfo))
      hidingId x = if (x == "Prelude") then ("import Prelude hiding (id)") else (T.append "import " x)
      mname = moduleNameForRecord rname
  writeModule FTModel mname ([qc|\{-# LANGUAGE TemplateHaskell, DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies #-}
\{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module {mname}
  (
    {T.intercalate ", " $ Set.toList $ Set.map (T.append "module ") (exportList tinfo)}
  , {T.intercalate ", " $ DL.map (T.append "module ") qualClassNames}
  )
where
import GHC.Generics
import Opaleye (Column, Table(..), Nullable, required, optional)
import qualified Data.Profunctor         as P
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Profunctor.Product (ProductProfunctor)
import Models.{rname}.Types
{T.unlines $ DL.map (T.append "import ") qualClassNames}
{T.unlines $ DL.map hidingId typeModules}
{recordTemplate tinfo}|])
  pure mname
-}

-- data ConcreteRecordInfo = ConcreteRecordInfo
--   { crName :: Text
--   , crFieldTypes :: [(FieldName, QualifiedType)]
--   }

-- data FieldPresence = FieldRequiredAlways
--                    | FieldOptionalAlways
--                    | FieldOptionalDuringCreation
--                    deriving (Eq, Show)


typeVariableName :: Text -> FieldInfo -> Text
typeVariableName suffix FieldInfo{..} = T.append fieldName suffix

typeVariableNames :: [FieldInfo] -> Maybe Text -> [Text]
typeVariableNames finfos mSuffix = case mSuffix of
  Nothing -> DL.map (typeVariableName "") finfos
  Just suffix -> DL.map (typeVariableName suffix) finfos

generateRecord :: RecordInfo -> CodeSnippet
generateRecord  rinfo@RecordInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = withNoConstructors <$> recordDeriving
  , newIdentifiers = [recordPolymorphicTypeName <> "(..)"]
  }
  where
    derivingTemplate = T.intercalate ", " $ DL.map qualifiedName recordDeriving
    fieldTemplate =
      T.intercalate ", " $
      (flip DL.map) recordFields $ \finfo ->
                                     T.concat [ prefixedFieldName rinfo finfo
                                              , if recordStrictFields then " :: " else ":: !"
                                              , typeVariableName "" finfo]
    snippet = ([qc|data {recordPolymorphicTypeName} {T.intercalate " " $ typeVariableNames recordFields Nothing} = {recordConstructurName} \{{fieldTemplate}} deriving ({derivingTemplate})
|])

withHaskellWrappers
  :: (HasCallStack)
  => (FieldInfo -> (ColIsNullable, QualifiedType))
  -> FieldInfo
  -> CodeSnippet
withHaskellWrappers accessorFn finfo@FieldInfo{..} = CodeSnippet
  { codeSnippet = (nullableWrapper . (arrayWrapper fieldArrayDim)) (T.concat ["(" , qualifiedName qname, ")"])
  , importList = [withAllConstructors qname]
  , newIdentifiers = ["module " <> (qModuleName qname)]
  }
  where
    (ColIsNullable colNullable, qname) = accessorFn finfo
    arrayWrapper n (t :: Text) = if n>0
                         then arrayWrapper (n-1) ([qc|[{t}]|])
                         else t
    nullableWrapper (t :: Text) = if colNullable then ([qc|(Prelude.Maybe {t})|])  else t

withPGWrappersInternal
  :: (HasCallStack)
  => (FieldInfo -> (ColIsNullable, ColHasDefault, QualifiedType))
  -> FieldInfo
  -> CodeSnippet
withPGWrappersInternal accessorFn finfo@FieldInfo{..} = CodeSnippet
  { codeSnippet = (defaultWrapper . columnWrapper . nullableWrapper . (arrayWrapper fieldArrayDim)) (T.concat ["(" , qualifiedName qname, ")"])
  , importList = [ withNoConstructors qname
                 , withNoConstructors $ QualifiedType "Opaleye" "Column"
                 ]
  , newIdentifiers = ["module " <> (qModuleName qname)]
  }
  where
    (ColIsNullable isNullable, ColHasDefault hasDefault, qname) = accessorFn finfo
    arrayWrapper n (t :: Text) = if n>0
                         then arrayWrapper (n-1) ([qc|(Opaleye.PGArray {t})|])
                         else t
    nullableWrapper (t :: Text) = if isNullable then ([qc|(Opaleye.Nullable {t})|]) else t
    columnWrapper (t :: Text) = ([qc|(Opaleye.Column {t})|])
    defaultWrapper (t :: Text) = if hasDefault then ([qc|(Prelude.Maybe {t})|]) else t

withPGReadWrappers
  :: (HasCallStack)
  => FieldInfo
  -> CodeSnippet
withPGReadWrappers finfo@FieldInfo{fieldPGReadType=(isNullable, qname)} =
  withPGWrappersInternal (const (isNullable, ColHasDefault False, qname)) finfo

withPGWriteWrappers
  :: (HasCallStack)
  => FieldInfo
  -> CodeSnippet
withPGWriteWrappers finfo = withPGWrappersInternal fieldPGWriteType finfo

generateConcreteRecordType
  :: (HasCallStack)
  => RecordInfo
  -> (FieldInfo -> CodeSnippet)
  -> Text
  -> CodeSnippet
generateConcreteRecordType RecordInfo{..} wrapperFn tyName = CodeSnippet
  { codeSnippet = ([qc|type {tyName} = {recordPolymorphicTypeName} {concreteTypes}|])
  , importList = [ withNoConstructors $ QualifiedType "Opaleye" "PGArray"
                 , withNoConstructors $ QualifiedType "Opaleye" "Nullable"
                 , withNoConstructors $ QualifiedType "Prelude" "Maybe"
                 ] ++ (ilist)
  , newIdentifiers = tyName:elist
  }
  where
    snippets = DL.map wrapperFn recordFields
    concreteTypes = T.intercalate " "  $ DL.map codeSnippet snippets
    ilist = DL.concatMap importList snippets
    elist = DL.concatMap newIdentifiers snippets

prefixedFieldName :: RecordInfo -> FieldInfo -> Text
prefixedFieldName RecordInfo{..} FieldInfo{..} = T.concat [recordFieldPrefix, (capitalizeFirstChar fieldName)]

prefixedFieldNames :: RecordInfo -> [Text]
prefixedFieldNames rinfo@RecordInfo{..} = DL.map (prefixedFieldName rinfo) recordFields

generateOpaleyeTable :: RecordInfo -> CodeSnippet
generateOpaleyeTable rinfo@RecordInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withNoConstructors lmapTy
                 , withAllConstructors tableTy
                 , withNoConstructors optionalTy
                 , withNoConstructors requiredTy
                 , withNoConstructors $ QualifiedType "Prelude" "(<$>)"
                 , withNoConstructors $ QualifiedType "Prelude" "(<*>)"
                 ]
  , newIdentifiers = [recordOpaleyeTableName]
  }
  where
    lmapTy = QualifiedType "Data.Profunctor" "lmap"
    tableTy = QualifiedType "Opaleye" "Table"
    optionalTy = QualifiedType "Opaleye" "optional"
    requiredTy = QualifiedType "Opaleye" "required"
    tableLMaps = DL.map fieldLMap recordFields
    fieldLMap finfo@FieldInfo{..} =
      let (ColIsNullable _, ColHasDefault hasDefault, _) = fieldPGWriteType
      in T.intercalate " " [ qualifiedName lmapTy
                           , prefixedFieldName rinfo finfo
                           , if hasDefault
                             then ([qc|({qualifiedName optionalTy} "{fieldPGName}")|])
                             else ([qc|({qualifiedName requiredTy} "{fieldPGName}")|])]
    snippet = ([qc|
{recordOpaleyeTableName} :: {qualifiedName tableTy} {recordPGWriteTypeName} {recordPGReadTypeName}
{recordOpaleyeTableName} = {qualifiedName tableTy} "{recordPGTableName}" ({recordConstructurName} Prelude.<$> {T.intercalate " Prelude.<*> " tableLMaps})
|])

generateProductProfunctor :: RecordInfo -> CodeSnippet
generateProductProfunctor rinfo@RecordInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withNoConstructors productProfunctorTy
                 , withNoConstructors defaultTy
                 , withNoConstructors defTy
                 , withNoConstructors lmapTy
                 , withNoConstructors applicativeTy
                 ]
  , newIdentifiers = []
  }
  where
    productProfunctorTy = QualifiedType "Data.Profunctor.Product" "ProductProfunctor"
    defaultTy = QualifiedType "Data.Profunctor.Product.Default" "Default"
    defTy = QualifiedType "Data.Profunctor.Product.Default" "def"
    lmapTy = QualifiedType "Data.Profunctor" "lmap"
    applicativeTy = QualifiedType "Prelude" "Applicative"
    typeVariableNames0 = typeVariableNames recordFields (Just "0")
    typeVariableNames1 = typeVariableNames recordFields (Just "1")
    defaultConstraints =
      T.intercalate ", " $
      DL.zipWith (\t0 t1 -> ([qc|{qualifiedName defaultTy} p {t0} {t1}|])) typeVariableNames0 typeVariableNames1
    defLMaps = DL.map
               (\fn -> ([qc|{qualifiedName lmapTy} {fn} {qualifiedName defTy}|]))
               (prefixedFieldNames rinfo)
    snippet = ([qc|
instance ({qualifiedName productProfunctorTy} p, {qualifiedName applicativeTy} (p ({recordPolymorphicTypeName} {T.intercalate " " typeVariableNames0})), {defaultConstraints}) => {qualifiedName defaultTy} p ({recordPolymorphicTypeName} {T.intercalate " " typeVariableNames0}) ({recordPolymorphicTypeName} {T.intercalate " " typeVariableNames1}) where
  def = {recordPolymorphicTypeName} Prelude.<$> {T.intercalate " Prelude.<*> " defLMaps}
|])

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
      DL.map (\(_, constrMap) -> ([qc|import {mname} ({T.intercalate ", " $ importItemWithConstructors constrMap})|])) $
      Map.toList importMap

{-

:{
generateLenses RecordInfo
{ recordPolymorphicTypeName = "Booking"
, recordConstructurName = "BookingPoly"
, recordFieldPrefix = "_booking"
, recordFields =
  [ FieldInfo {fieldName = "id", fieldPGName = "id", fieldRequired = True, fieldHaskellReadType = (QualifiedType "Prelude" "Int"), fieldHaskellWriteType = (QualifiedType "Prelude" "()"), fieldPGWriteType = (QualifiedType "Prelude" "()"), fieldPGReadType = (QualifiedType "Opaleye" "PGInt4")}
  , FieldInfo {fieldName = "created_at", fieldPGName = "created_at", fieldRequired = True, fieldHaskellReadType = (QualifiedType "Date" "UTCTime"), fieldHaskellWriteType = (QualifiedType "Prelude" "()"), fieldPGWriteType = (QualifiedType "Prelude" "()"), fieldPGReadType = (QualifiedType "Opaleye" "PGTimestampTz")}
  , FieldInfo {fieldName = "updated_at", fieldPGName = "updated_at", fieldRequired = True, fieldHaskellReadType = (QualifiedType "Date" "UTCTime"), fieldHaskellWriteType = (QualifiedType "Prelude" "()"), fieldPGWriteType = (QualifiedType "Prelude" "()"), fieldPGReadType = (QualifiedType "Opaleye" "PGTimestampTz")}
  ]
, recordDeriving = [QualifiedType "Prelude" "Eq", QualifiedType "Prelude" "Show"]
, recordHaskellReadTypeName = "Booking"
, recordHaskellWriteTypeName = "BookingW"
, recordPGReadTypeName = "BookingPGR"
, recordPGWriteTypeName = "BookingPGW"
, recordOpaleyeTableName = "tableForBooking"
, recordPGTableName = "itinerary_bookings"
}
:}


-}

{-
recordTemplate :: TableInfo -> Text
recordTemplate tinfo = ([qc|data {pname} {typeVarString} = {pname} \{{fieldTemplate}} deriving (Eq, Show, Generic)
type {rname} = {pname} {prepareTypeString withHaskellWrappers}
type {rname}W = {pname} {writeProtectedTypeString}
type {rname}PGW = {pname} {prepareTypeString withPGWriteWrappers}
type {rname}PGR = {pname} {prepareTypeString withPGReadWrappers}
{T.unlines $ DL.map lensTemplate sortedCInfos}

{oname} :: Table {rname}PGW {rname}PGR
{oname} = Table "{tableName tinfo}" $ {polyConstructorName tinfo} <$> {T.intercalate " <*> " tableLMaps}

instance (ProductProfunctor p, Applicative (p ({pname} {typeVarStringN "0"})), {defaultConstraints}) => Default p ({pname} {typeVarStringN "0"}) ({pname} {typeVarStringN "1"}) where
  def = {pname} <$> {T.intercalate " <*> " defLMaps}
|])
  where
    prepareTypeString wrapper = T.intercalate " " $ DL.map (\cinfo -> wrapper cinfo) sortedCInfos
    rname = recordName tinfo
    pname = polyConstructorName tinfo
    oname = opaleyeTableName tinfo
    sortedCInfos = (DL.sortBy (\x y -> compare (colPosition x) (colPosition y)) $ Map.elems $ columnMap tinfo)
    writeProtectedTypeString = T.intercalate " " $
      (flip DL.map) sortedCInfos $ \cinfo -> if (DL.elem (colName cinfo) (writeProtectedColumns tinfo))
                                             then "()"
                                             else (withHaskellWrappers cinfo)
    fieldTemplate = T.intercalate ", "
                    (DL.map
                     (\tv -> ([qc|{disambiguatedFieldName tv} :: {tv}|]))
                     (snd <$> typeVariables))
    disambiguatedFieldName tv = T.concat [recordPrefix tinfo, (capitalizeFirstChar tv)]
    typeVariables = DL.map (\x -> (colPosition x, colFieldName x)) sortedCInfos
    typeVarString = T.intercalate " " (snd <$> typeVariables)
    typeVarStringN n = T.intercalate " " (DL.map (\x -> T.append x n) (snd <$> typeVariables))
    defaultConstraints = T.intercalate ", " (DL.map (\tv -> ([qc|Default p {T.append tv "0"} {T.append tv "1"}|])) (snd <$> typeVariables))
    lensTemplate cinfo =
      let colpos = colPosition cinfo
          varname = "dragon"
          targetField = fromJustNote ([qc|Could not find col with position {colpos} in {tinfo}|]) $ DL.lookup colpos typeVariables
          newConstr = T.intercalate " " $ (DL.map (\(cpos, cname) -> if cpos==colpos then varname else cname)) typeVariables
          fname = colFieldName cinfo
      in ([qc|instance Has{fieldToClassName fname} ({pname} {typeVarString}) {targetField} where
  \{-# INLINE {fname} #-}
  {fname} fctor ({pname} {typeVarString}) = fmap (\ {varname} -> {pname} {newConstr}) (fctor {targetField})|])

    tableLMaps = (flip DL.map) sortedCInfos
                 (\cinfo -> T.intercalate " " ["P.lmap"
                                              , disambiguatedFieldName $ colFieldName cinfo
                                              , if (colDefault cinfo)
                                                then ([qc|(optional "{colName cinfo}")|])
                                                else ([qc|(required "{colName cinfo}")|])])

    defLMaps = DL.map
               (\cinfo -> T.intercalate " " ["P.lmap", (disambiguatedFieldName $ colFieldName cinfo), "def"])
               sortedCInfos
-}
