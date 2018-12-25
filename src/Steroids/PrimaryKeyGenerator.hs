{-# LANGUAGE RecordWildCards #-}
module Steroids.PrimaryKeyGenerator where

import Steroids.Types
import Data.Text as T
import Text.InterpolatedString.Perl6 (qc)
import GHC.Stack
import Data.Maybe (mapMaybe)
import Control.Lens
import qualified Data.List as DL


generatePhantom
  :: (HasCallStack)
  => PhantomTypeInfo
  -> CodeSnippet
generatePhantom PhantomTypeInfo{..} = CodeSnippet
  { codeSnippet = snippet
  , importList = [ withAllConstructors phPhantomType
                 , withNoConstructors phCoreType
                 ]
  , newIdentifiers = [pkName]
  }
  where
    pkName = qTypeName phSynonym
    ghostType = T.append pkName "Ghost"
    snippet = ([qc|data {ghostType}
type {pkName} = {qualifiedName phPhantomType} {qualifiedName phCoreType} {ghostType}
|])



extractPkSettings
  :: (HasCallStack)
  => [RecordInfo]
  -> ([NewtypeInfo], [PhantomTypeInfo])
extractPkSettings rinfos =
  ( DL.concatMap (simpleNewtypes . recordFields ) rinfos
  , DL.concatMap (phantomTypes . recordFields ) rinfos
  )
  where
    simpleNewtypes = mapMaybe ((^? _SimpleNewtype) . fieldPkSetting)
    phantomTypes = mapMaybe ((^? _PhantomNewtype) . fieldPkSetting)
