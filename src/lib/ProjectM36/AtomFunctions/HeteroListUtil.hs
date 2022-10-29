{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# Language UndecidableSuperClasses #-}
{-# Language ConstraintKinds #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
module ProjectM36.AtomFunctions.HeteroListUtil where
import Data.HeteroList
import Data.HList(HList(..))

-- * Convert between heterogeneous lists and homogeneous ones

class HList2List l e | l -> e
 where
  hList2List :: HeteroList l -> [e]
  list2HListSuffix :: [e] -> Maybe (HeteroList l, [e])


list2HList :: HList2List l e => [e] -> Maybe (HeteroList l)
list2HList = fmap fst . list2HListSuffix


instance HList2List '[e] e
 where
  hList2List ((:-) e Nil) = [e]

  list2HListSuffix (e : es) = Just ((:-) e Nil, es)
  list2HListSuffix [] = Nothing


instance HList2List (e' ': l) e
      => HList2List (e ': e' ': l) e
 where
  hList2List ((:-) e l) = e:hList2List l

  list2HListSuffix (e : es) = (\(hl,rest) -> ((:-) e hl, rest))
                                  <$> list2HListSuffix es
  list2HListSuffix [] = Nothing



htrList2HList :: HeteroList l -> HList l
htrList2HList ((:-) x Nil) = HCons x HNil
htrList2HList ((:-) x y) = HCons x (htrList2HList y)
 
