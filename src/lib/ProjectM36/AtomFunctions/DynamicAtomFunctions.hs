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
{-# LANGUAGE GADTs #-}
module ProjectM36.AtomFunctions.DynamicAtomFunctions where
import Prelude
import ProjectM36.Base
import ProjectM36.Atomable
import ProjectM36.AtomFunction
import Data.Typeable
import ProjectM36.AtomFunctions.HeteroListUtil as HeteroListUtil
import Data.HeteroList
import Data.Poly
import Data.Poly.Function (mkPolyFunc1, Equal)
import Control.IndexT.Function
import Control.ConstraintManip
import Data.HList hiding (apply)
import Data.HList.HCurry
import Data.Maybe
import qualified Data.Function.Poly as FP
import GHC.TypeLits
import Generics.SOP (All)
import Data.Tuple.Curry
import Data.Tuple.HList
import Data.Tuple.Solo
import qualified Data.Text as T

genAtomFunc:: forall f.(
  All Atomable (FP.ArityToTypeList f),
  Atomable (FP.Result f),
  HeteroListUtil.HList2List (MapConst Atom (FP.ArityToTypeList f)) Atom,
  Data.HeteroList.HeteroMapConstraint
  ((IsFunc 1 &&& IxConstrainBy (Arg 0) (Equal Atom)) &&& IxConstrainBy (Result 1) Atomable)
  (MapConst Atom (FP.ArityToTypeList f))
  (FP.ArityToTypeList f),
  HLst (Tuple (FP.ArityToTypeList f))  (HList (FP.ArityToTypeList f)),
  Curry (Tuple (FP.ArityToTypeList f) -> FP.Result f) f,
  AtomTypeList (FP.Append (FP.ArityToTypeList f) (FP.Result f)))=> T.Text -> f -> AtomFunction
genAtomFunc name f = Function{
  funcName = name,
  funcType = atomTypeList @(FunctionTypeList f),
  funcBody = FunctionBuiltInBody (\xs -> pure $ apply f xs)}


type FunctionTypeList f = FP.Append (FP.ArityToTypeList f) (FP.Result f)

type DeriveAtomTypeList (xs :: [*])  = [AtomType]
class AtomTypeList (xs :: [*]) where
  atomTypeList :: DeriveAtomTypeList xs
instance AtomTypeList '[] where
  atomTypeList = []
instance (Atomable a, AtomTypeList xs) => AtomTypeList (a ': xs) where
  atomTypeList = (toAtomType @a Proxy) : (atomTypeList @xs)

type family Length (xs :: [*]) :: HNat where
  Length '[] = HZero
  Length (x ': xs) = HSucc (Length xs)

type family Tuple (xs :: [*]) :: * where
  Tuple '[] = TypeError (Text "impossible HListToTuple")
  Tuple (a1 ': '[]) = Solo a1
  Tuple (a1 ': a2 ': '[]) = (a1, a2)
  Tuple (a1 ': a2 ': a3 ': '[]) = (a1, a2, a3)
  Tuple (a1 ': a2 ': a3 ': a4 ': '[]) = (a1, a2, a3, a4)
  Tuple (a1 ': a2 ': a3 ': a4 ': a5 ': '[]) = (a1, a2, a3, a4, a5)

type family MapConst (a :: *) (xs :: [*]) :: [*] where
  MapConst ty '[] = '[]
  MapConst ty (x ': xs) = ty ': (MapConst ty xs)

-- ...mkPolyFunc1... is an unknown symbol in .so file
-- I think I use same version of project-m36, cause I nix-shell to get ghc-with-project-m36 and tutd at the same time.
polyFromAtom = mkPolyFunc1 @(Equal Atom) @(Atomable) fromAtom

--  use uncurryN to apply a function
apply :: forall f.(
  All Atomable (FP.ArityToTypeList f),
  Atomable (FP.Result f),
  HeteroListUtil.HList2List (MapConst Atom (FP.ArityToTypeList f)) Atom,
  Data.HeteroList.HeteroMapConstraint
  ((IsFunc 1 &&& IxConstrainBy (Arg 0) (Equal Atom)) &&& IxConstrainBy (Result 1) Atomable)
  (MapConst Atom (FP.ArityToTypeList f))
  (FP.ArityToTypeList f),
  HLst (Tuple (FP.ArityToTypeList f))  (HList (FP.ArityToTypeList f)),
  Curry (Tuple (FP.ArityToTypeList f) -> FP.Result f) f)=>
  f -> [Atom] -> Atom
apply f atoms =
  let atomsHtrList = fromJust (HeteroListUtil.list2HList atoms :: Maybe (HeteroList (MapConst Atom (FP.ArityToTypeList f))))
      htrList = hmap polyFromAtom atomsHtrList :: HeteroList (FP.ArityToTypeList f)
      hlist = htrList2HList htrList
      argsTup = fromHList hlist :: Tuple (FP.ArityToTypeList f)
  in toAtom @(FP.Result f) $ uncurryN f argsTup

