{-# LANGUAGE RecordWildCards #-}
module Steroids.EnumGenerator where

import Steroids.Types
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

data EnumInfo = EnumInfo
  { enumType :: !QualifiedType
  , enumValues :: ![(Text, Text)]
  , enumDeriving :: ![QualifiedType]
  , enumToStringFunction :: !Text
  , enumFromStringFunction :: !Text
  } deriving (Eq, Show)

generateEnumData
  :: (HasCallStack)
  => EnumInfo
  -> CodeSnippet
generateEnumData EnumInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = withNoConstructors <$> enumDeriving
  , newIdentifiers = [tname <> "(..)"]
  }
  where
    (QualifiedType mname tname) = enumType
    constrString = T.intercalate " | " $ DL.map fst enumValues
    derivingString = case enumDeriving of
      [] -> "" :: Text
      x -> ([qc|deriving ({T.intercalate ", " $ DL.map qualifiedName x})|])
    snippet = ([qc|data {tname} = {constrString} {derivingString}|])

generateStringInstances
  :: (HasCallStack)
  => EnumInfo
  -> CodeSnippet
generateStringInstances EnumInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withAllConstructors $ QualifiedType "Data.String" "IsString"
                 , withNoConstructors $ QualifiedType "Prelude" "error"
                 , withNoConstructors $ QualifiedType "Prelude" "($)"
                 , withNoConstructors $ QualifiedType "Prelude" "(++)"
                 , withNoConstructors $ QualifiedType "Prelude" "show"
                 , withNoConstructors $ QualifiedType "Prelude" "pure"
                 , withNoConstructors $ QualifiedType "Prelude" "(.)"
                 ]
  , newIdentifiers = []
  }
  where
    (QualifiedType mname tname) = enumType
    -- NOTE: leading two spaces are very important!
    haskellToSTemplate (constructor :: Text, label :: Text) = ([qc|  {constructor} -> "{label}"|])

    -- NOTE: leading two spaces are very important!
    sToHaskellTemplate (constructor :: Text, label :: Text) = ([qc|  "{label}" -> {constructor}|])


    snippet = ([qc|
{enumToStringFunction} :: (Data.String.IsString s) => {tname} -> s
{enumToStringFunction} x = case x of
{T.unlines Prelude.$ DL.map haskellToSTemplate enumValues}

{enumFromStringFunction} :: (Prelude.Eq s, Prelude.Show s, Data.String.IsString s) => s -> {tname}
{enumFromStringFunction} x = case x of
{T.unlines $ DL.map sToHaskellTemplate enumValues}
  y -> Prelude.error Prelude.$ "Unexpected value while parsing {mname}.{tname}: " Prelude.++ (Prelude.show y)
|])


generateJsonInstances
  :: (HasCallStack)
  => EnumInfo
  -> CodeSnippet
generateJsonInstances EnumInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withAllConstructors $ QualifiedType "Data.Aeson" "FromJSON"
                 , withAllConstructors $ QualifiedType "Data.Aeson" "ToJSON"
                 , withNoConstructors $ QualifiedType "Data.Aeson" "withText"
                 , withNoConstructors $ QualifiedType "Data.Text" "Text"
                 ]
  , newIdentifiers = []
  }
  where
    (QualifiedType mname tname) = enumType
    snippet = ([qc|
instance Data.Aeson.FromJSON {tname} where
  parseJSON = Data.Aeson.withText "Expecting a string for {mname}.{tname}" (Prelude.pure Prelude.. {enumFromStringFunction})

instance Data.Aeson.ToJSON {tname} where
  toJSON x = Data.Aeson.toJSON ({enumToStringFunction} x :: Data.Text.Text)
|])

generateDbInstances
  :: (HasCallStack)
  => EnumInfo
  -> CodeSnippet
generateDbInstances EnumInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withAllConstructors fromFieldTy
                 , withAllConstructors toFieldTy
                 , withAllConstructors queryRunnerColumnDefaultTy
                 , withNoConstructors columnTy
                 , withAllConstructors constantTy
                 , withNoConstructors fieldQueryRunnerColumnTy
                 , withNoConstructors returnErrorTy
                 , withNoConstructors maybeTy
                 , withAllConstructors resultErrorTy
                 , withNoConstructors textTy
                 , withAllConstructors defaultTy
                 , withNoConstructors pgStrictTextTy
                 , withNoConstructors unsafeCoerceTy
                 ]
  , newIdentifiers = []
  }
  where
    (QualifiedType mname tname) = enumType
    fromFieldTy = QualifiedType "Database.PostgreSQL.Simple.FromField" "FromField"
    toFieldTy = QualifiedType "Database.PostgreSQL.Simple.ToField" "ToField"
    queryRunnerColumnDefaultTy = QualifiedType "Opaleye" "QueryRunnerColumnDefault"
    columnTy = QualifiedType "Opaleye" "Column"
    constantTy = QualifiedType "Opaleye" "Constant"
    fieldQueryRunnerColumnTy = QualifiedType "Opaleye" "fieldQueryRunnerColumn"
    returnErrorTy = QualifiedType "Database.PostgreSQL.Simple.FromField" "returnError"
    resultErrorTy = QualifiedType "Database.PostgreSQL.Simple.FromField" "ResultError"
    unexpectedNullTy = QualifiedType "Database.PostgreSQL.Simple.FromField" "UnexpectedNull"
    maybeTy = QualifiedType "Data.Maybe" "maybe"
    textTy = QualifiedType "Data.Text" "Text"
    defaultTy = QualifiedType "Data.Profunctor.Product.Default" "Default"
    pgStrictTextTy = QualifiedType "Opaleye" "pgStrictText"
    unsafeCoerceTy = QualifiedType "Opaleye" "unsafeCoerceColumn"
    snippet = ([qc|
instance {qualifiedName fromFieldTy} {tname} where
  fromField f mBS = {qualifiedName maybeTy} ({qualifiedName returnErrorTy} {qualifiedName unexpectedNullTy} f "Unexpected NULL while parsing {mname}.{tname} from the DB") (Prelude.pure Prelude.. {enumFromStringFunction}) mBS

instance {qualifiedName toFieldTy} {tname} where
  toField x = Database.PostgreSQL.Simple.ToField.toField Prelude.$ ({enumToStringFunction} x :: {qualifiedName textTy})

instance {qualifiedName queryRunnerColumnDefaultTy} {tname} {tname} where
  queryRunnerColumnDefault = {qualifiedName fieldQueryRunnerColumnTy}

instance {qualifiedName defaultTy} {qualifiedName constantTy} {tname} ({qualifiedName columnTy} {tname}) where
  def = {qualifiedName constantTy} Prelude.$ \x -> {qualifiedName unsafeCoerceTy} Prelude.$ {qualifiedName pgStrictTextTy} Prelude.$ {enumToStringFunction} x
|])
