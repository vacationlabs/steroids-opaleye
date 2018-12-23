{-# LANGUAGE RecordWildCards #-}
module Steroids.PrimaryKeyGenerator where

import Steroids.Types
import Data.Text as T
import Text.InterpolatedString.Perl6 (qc)
import GHC.Stack


generatePrimaryKey
  :: (HasCallStack)
  => QualifiedType
  -> CodeSnippet
generatePrimaryKey (QualifiedType _ pkName) = CodeSnippet
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

