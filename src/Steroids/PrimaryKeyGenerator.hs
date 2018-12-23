{-# LANGUAGE RecordWildCards #-}
module Steroids.PrimaryKeyGenerator where

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


generatePrimaryKey
  :: (HasCallStack)
  => QualifiedType
  -> CodeSnippet
generatePrimaryKey (QualifiedType mname pkName) = CodeSnippet
  { codeSnippet = snippet
  , importList = [withAllConstructors $ QualifiedType "Foundation.Types.PrimaryKey" "PK"]
  , newIdentifiers = [pkName]
  }
  where
    ghostType = T.append pkName "Ghost"
    snippet = ([qc|
data {ghostType}
type {pkName} = Foundation.Types.PrimaryKey.PK {ghostType}
|])

