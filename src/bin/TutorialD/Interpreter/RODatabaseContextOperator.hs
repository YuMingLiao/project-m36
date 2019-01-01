{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.RODatabaseContextOperator where
import ProjectM36.Base
import ProjectM36.Attribute (attributeForName) 
import ProjectM36.DataFrame
import ProjectM36.Relation (attributes)
import ProjectM36.Error
import ProjectM36.InclusionDependency
import qualified ProjectM36.Client as C
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextExpr
import Control.Monad.State
import qualified Data.Text as T
import ProjectM36.Relation.Show.Gnuplot
import qualified Data.Map as M
import Data.Maybe

--operators which only rely on database context reading
data RODatabaseContextOperator where
  ShowRelation :: RelationalExpr -> RODatabaseContextOperator
  PlotRelation :: RelationalExpr -> RODatabaseContextOperator
  ShowRelationType :: RelationalExpr -> RODatabaseContextOperator
  ShowConstraint :: StringType -> RODatabaseContextOperator
  ShowPlan :: DatabaseContextExpr -> RODatabaseContextOperator
  ShowTypes :: RODatabaseContextOperator
  ShowRelationVariables :: RODatabaseContextOperator
  ShowAtomFunctions :: RODatabaseContextOperator
  ShowDatabaseContextFunctions :: RODatabaseContextOperator
  ShowDataFrame :: RelationalExpr -> [AttributeOrderExpr] -> Maybe Integer -> Maybe Integer -> RODatabaseContextOperator
  Quit :: RODatabaseContextOperator
  deriving (Show)

typeP :: Parser RODatabaseContextOperator
typeP = do
  reservedOp ":type"
  ShowRelationType <$> relExprP

showRelP :: Parser RODatabaseContextOperator
showRelP = do
  reservedOp ":showexpr"
  ShowRelation <$> relExprP

showPlanP :: Parser RODatabaseContextOperator
showPlanP = do
  reservedOp ":showplan"
  ShowPlan <$> databaseContextExprP

showTypesP :: Parser RODatabaseContextOperator
showTypesP = reserved ":showtypes" >> pure ShowTypes

showRelationVariables :: Parser RODatabaseContextOperator
showRelationVariables = reserved ":showrelvars" >> pure ShowRelationVariables

showAtomFunctionsP :: Parser RODatabaseContextOperator
showAtomFunctionsP = reserved ":showatomfunctions" >> pure ShowAtomFunctions

showDatabaseContextFunctionsP :: Parser RODatabaseContextOperator
showDatabaseContextFunctionsP = reserved ":showdatabasecontextfunctions" >> pure ShowDatabaseContextFunctions

quitP :: Parser RODatabaseContextOperator
quitP = do
  reservedOp ":quit"
  return Quit

showConstraintsP :: Parser RODatabaseContextOperator
showConstraintsP = do
  reservedOp ":constraints"
  ShowConstraint <$> option "" identifier
  
plotRelExprP :: Parser RODatabaseContextOperator  
plotRelExprP = do
  reserved ":plotexpr"
  PlotRelation <$> relExprP

roDatabaseContextOperatorP :: Parser RODatabaseContextOperator
roDatabaseContextOperatorP = typeP
             <|> showRelP
             <|> showRelationVariables
             <|> plotRelExprP
             <|> showConstraintsP
             <|> showPlanP
             <|> showTypesP
             <|> showAtomFunctionsP
             <|> showDatabaseContextFunctionsP
             <|> showDataFrameP
             <|> quitP

--logically, these read-only operations could happen purely, but not if a remote call is required
evalRODatabaseContextOp :: C.SessionId -> C.Connection -> RODatabaseContextOperator -> IO TutorialDOperatorResult
evalRODatabaseContextOp sessionId conn (ShowRelationType expr) = do
  res <- C.typeForRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> pure $ DisplayRelationResult rel

evalRODatabaseContextOp sessionId conn (ShowRelation expr) = do
  res <- C.executeRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> pure $ DisplayRelationResult rel
    
evalRODatabaseContextOp sessionId conn (PlotRelation expr) = do
  res <- C.executeRelationalExpr sessionId conn expr
  pure $ case res of
    Left err -> DisplayErrorResult $ T.pack (show err)
    Right rel -> DisplayIOResult $ do
      err <- plotRelation rel
      when (isJust err) $ print err

evalRODatabaseContextOp sessionId conn (ShowConstraint name) = do
  eIncDeps <- C.inclusionDependencies sessionId conn
  let val = case eIncDeps of
        Left err -> Left err
        Right incDeps -> case name of
          "" -> inclusionDependenciesAsRelation incDeps
          depName -> case M.lookup depName incDeps of
            Nothing -> Left (InclusionDependencyNameNotInUseError depName)
            Just dep -> inclusionDependenciesAsRelation (M.singleton depName dep)
  pure $ case val of
     Left err -> DisplayErrorResult (T.pack (show err))
     Right rel -> DisplayRelationResult rel

evalRODatabaseContextOp sessionId conn (ShowPlan dbExpr) = do
  plan <- C.planForDatabaseContextExpr sessionId conn dbExpr
  pure $ case plan of 
    Left err -> DisplayErrorResult (T.pack (show err))
    Right optDbExpr -> DisplayResult $ T.pack (show optDbExpr)

evalRODatabaseContextOp sessionId conn ShowTypes = do  
  eRel <- C.atomTypesAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
    
evalRODatabaseContextOp sessionId conn ShowRelationVariables = do
  eRel <- C.relationVariablesAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
    
evalRODatabaseContextOp sessionId conn ShowAtomFunctions = do
  eRel <- C.atomFunctionsAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
    
evalRODatabaseContextOp sessionId conn ShowDatabaseContextFunctions = do
  eRel <- C.databaseContextFunctionsAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
  
evalRODatabaseContextOp sessionId conn (ShowDataFrame expr attrOrdersExpr mbOffset mbLimit) = do
  res <- C.executeRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> do
      let relAttrs = attributes rel
          attrName (AttributeOrderExpr name _) = name
          order (AttributeOrderExpr _ ord) = ord
          orders = map order attrOrdersExpr
          attributeForName' = flip attributeForName relAttrs 
          attrNames = map attrName attrOrdersExpr
          verified = forM attrNames attributeForName'
      case verified of
        Left err -> pure $ DisplayErrorResult $ T.pack (show err)
        Right attrs -> do
              let attrOrders = map (\(attr',order')->AttributeOrder attr' order') (zip attrs orders)
              let dFrame = sortDataFrameBy attrOrders . toDataFrame $ rel
              let dFrame' = maybe dFrame (`drop'` dFrame) mbOffset
              let dFrame'' = maybe dFrame' (`take'` dFrame') mbLimit
              pure $ DisplayDataFrameResult $ dFrame''

 
evalRODatabaseContextOp _ _ Quit = pure QuitResult

interpretRODatabaseContextOp :: C.SessionId -> C.Connection -> T.Text -> IO TutorialDOperatorResult
interpretRODatabaseContextOp sessionId conn tutdstring = case parse roDatabaseContextOperatorP "" tutdstring of
  Left err -> pure $ DisplayErrorResult (T.pack (show err))
  Right parsed -> evalRODatabaseContextOp sessionId conn parsed
  
showDataFrameP :: Parser RODatabaseContextOperator
showDataFrameP = do
  reservedOp ":showdataframe"
  relExpr <- relExprP
  reservedOp "orderby"
  attrOrdersExpr <- attrOrdersExprP
  mbOffset <- optional offsetP
  mbLimit <- optional limitP
  pure $ ShowDataFrame relExpr attrOrdersExpr mbOffset mbLimit


offsetP :: Parser Integer
offsetP = do
  reservedOp "offset"
  integer

limitP :: Parser Integer
limitP = do
  reservedOp "limit"
  integer

attrOrdersExprP :: Parser [AttributeOrderExpr]
attrOrdersExprP = braces (sepBy attrOrderExprP comma)

attrOrderExprP :: Parser AttributeOrderExpr
attrOrderExprP = AttributeOrderExpr <$> identifier <*> orderP

orderP :: Parser Order
orderP = (try $ reservedOp "ASC" >> pure ASC) <|> (try $ reservedOp "DESC" >> pure DESC) <|> pure ASC
