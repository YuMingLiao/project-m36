module ProjectM36.Analyzer where
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List (nub)

relVarNames :: RelationalExprBase a -> [RelVarName]
relVarNames x = nub [y | RelationVariable y _ <- universe x]


