{-# LANGUAGE RecordWildCards #-}
module Steroids.LensClassGenerator where

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
import Steroids.RecordGenerator as Rec


generateLensClass
  :: (HasCallStack)
  => ModuleName
  -> FieldName
  -> Text
generateLensClass (ModuleName mname) (FieldName fname) = ([qc|\{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module {mname} where
import Control.Lens
class Has{capitalizeFirstChar fname} s a | s -> a where {fname} :: Lens' s a
|])

generateLenses :: RecordInfo -> CodeSnippet
generateLenses rinfo@RecordInfo{..} = CodeSnippet
  { codeSnippet = T.intercalate "\n" $ DL.map codeSnippet snippets
  , importList = DL.concatMap importList snippets
  , newIdentifiers = DL.concatMap newIdentifiers snippets
  }
  where
    tyNames = typeVariableNames recordFields Nothing
    varname = "dragon"
    snippets = DL.zipWith (generateLens rinfo tyNames varname) recordFields [0..]

generateLens :: RecordInfo -> [Text] -> Text -> FieldInfo -> Int -> CodeSnippet
generateLens RecordInfo{..} tyNames varname FieldInfo{..} fpos = CodeSnippet
  { codeSnippet = snippet
  , importList  = [ withAllConstructors qualClassName
                  , withNoConstructors $ QualifiedType "Prelude" "fmap"]
  , newIdentifiers = ["module " <> (qModuleName qualClassName)]
  }
  where
    newConstr = T.intercalate " " $ tyNames & (ix fpos) .~ varname
    typeVarString = T.intercalate " " tyNames
    qualClassName = QualifiedType ([qc|{lensClassParentModule}.{capitalizeFirstChar fieldName}|]) ([qc|Has{capitalizeFirstChar fieldName}|])
    snippet = ([qc|
instance {qualifiedName qualClassName} ({recordPolymorphicTypeName} {typeVarString}) {fieldName} where
  \{-# INLINE {fieldName} #-}
  {fieldName} fctor ({recordPolymorphicTypeName} {typeVarString}) = Prelude.fmap (\ {varname} -> {recordPolymorphicTypeName} {newConstr}) (fctor {fieldName})
  |])
