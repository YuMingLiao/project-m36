{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module ProjectM36.AtomFunctions.MyServerFunctions where
import ProjectM36.AtomFunctions.DynamicAtomFunctions
import Text.Read
import Data.Text 
import ProjectM36.Base
import ProjectM36.Atomable
import ProjectM36.AtomFunction
import GHC.Generics
import Control.DeepSeq
import Codec.Winery.Class
import Control.Applicative
import Data.Text
import Text.Fuzzy
import Data.Maybe
import Prelude  
import qualified Data.HashSet as HS

-- attempt to not pattern match on Atom and directly use haskell-land function
haskellFunctions:: AtomFunctions
haskellFunctions = HS.fromList
                   [genAtomFunc "plus" ((+) :: Int -> Int -> Int)
                   ,genAtomFunc "readMaybeInt" ((readMaybeInt) :: Text -> Maybe Int)
                   ,genAtomFunc "readMaybeInteger" ((readMaybeInteger) :: Text -> Maybe Integer)
                   ,genAtomFunc "isInfixOf" ((isInfixOf) :: Text -> Text -> Bool)
                   ,genAtomFunc "match" ((isMatch) :: Text -> Text -> Bool)
                   -- ,genAtomFunc "textLength" (length :: Text -> Int) -- segmentation fault, core dumped
                   -- ,genAtomFunc "textLength" (textLength :: Text -> Int) -- the same 
                   ,genAtomFunc "filterOutSpace" (Data.Text.filter (/=' ') :: Text -> Text)]
-- textLength = Prelude.length . unpack
readMaybeInt :: Text -> Maybe Int
readMaybeInt = readMaybe . unpack
readMaybeInteger :: Text -> Maybe Integer
readMaybeInteger = readMaybe . unpack
isMatch x y = isJust (match x y "" "" id False) 

