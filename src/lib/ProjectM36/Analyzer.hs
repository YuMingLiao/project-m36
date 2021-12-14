module ProjectM36.Analyzer where
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List (nub)
import ProjectM36.Base

relVarNames :: Data a => RelationalExprBase a -> [RelVarName]
relVarNames x = nub [y | RelationVariable y _ <- universe x]

-- max can be optimized by sorting first
-- count can be optimized by GraphRefRelationalExprFunction
-- project can be optimized by GraphRefRelationalExpr
-- cached can be saved at a place and changed by only GraphRef
-- 
-- two constriants / relational exprs 
--
-- cacheRE returns a cachedRE (It feels like add an record action in tail recursion, just put result in a cache instead return it. Orcall evalRE in all pattern matches then store the result)
-- cache constraints/ relExpr
-- diff constraints  if equal, same result
-- It's a equvalent replacement. apply optimization rules.
--   if ex. count(sp@1) != count(sp@1 union tuple) --relvars to be validated
--   count(sp@1 (union or minus) count(tuple) = count(sp@1) (+ or -) count(tuples)  --divide and conquer? recursively calculate?
--   recalculate constraints
--   sp@1{#s} => (sp@1 (union or minus) tuples){#s} = sp@1{#s} (union or minus) tuples{#s}
--   extend => sp@1 expand (union or minus) tuples extend
--   so it's how to get two side's value before eval union or minus.
--   max(sp@1{#s}) => max(sp@1{#s}) (union => whoIsBigger) max (tuples{#s})
--                                  (minus => remove elems in a linked list)
--   distribute functions or eval inner first?
--
--   evalREByLastCachedRE
--   recurse two ADTs to have a diff report. If same, return cached. If not, apply optimization rule and return re-calculated values.
--   cache doesn't eval GraphRefRA, either
--
--   cached should be stay with head?
