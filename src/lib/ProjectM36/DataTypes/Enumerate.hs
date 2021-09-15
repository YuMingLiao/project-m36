module ProjectM36.DataTypes.Enumerate where
import ProjectM36.Base
import Data.Map as M

isEnumerableAtomType :: AtomType -> Bool
isEnumerableAtomType typ = case typ of 
  BoolAtomType -> True 
  ConstructedAtomType _ tvmap -> M.null tvmap -- starts small
  _ -> False

{- 
enumerate :: AtomType -> [Atom]
enumerate typ = 
  case typ of
    BoolAtomType ->  map BoolAtom [True, False]
    ConstructedAtomType _ tvmap -> 
      case M.null tvmap of 
        True  -> []
        False -> []
    _ -> [] 
-}
