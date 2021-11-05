{-# LANGUAGE CPP #-}
module ProjectM36.DatabaseContextFunction where
--implements functions which operate as: [Atom] -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Serialise.Base ()
import ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.AtomType
import qualified Data.HashSet as HS
import qualified Data.Map as M
import ProjectM36.ScriptSession
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Codec.Winery

emptyDatabaseContextFunction :: DatabaseContextFunctionName -> DatabaseContextFunction
emptyDatabaseContextFunction name = DatabaseContextFunction { 
  dbcFuncName = name,
  dbcFuncType = [],
  dbcFuncBody = DatabaseContextFunctionBody Nothing (\_ ctx -> pure ctx)
  }

databaseContextFunctionForName :: DatabaseContextFunctionName -> DatabaseContextFunctions -> Either RelationalError DatabaseContextFunction
databaseContextFunctionForName funcName funcs = if HS.null foundFunc then
                                                   Left $ NoSuchFunctionError funcName
                                                else
                                                  Right (head (HS.toList foundFunc))
  where
    foundFunc = HS.filter (\(DatabaseContextFunction name _ _) -> name == funcName) funcs

evalDatabaseContextFunction :: DatabaseContextFunction -> [Atom] -> DatabaseContext -> Either RelationalError DatabaseContext
evalDatabaseContextFunction func args ctx = case dbcFuncBody func of
  (DatabaseContextFunctionBody _ f) -> case f args ctx of
    Left err -> Left (DatabaseContextFunctionUserError err)
    Right c -> pure c
  
basicDatabaseContextFunctions :: DatabaseContextFunctions
basicDatabaseContextFunctions = HS.fromList [
  DatabaseContextFunction { dbcFuncName = "deleteAll",
                            dbcFuncType = [],
                            dbcFuncBody = DatabaseContextFunctionBody Nothing (\_ ctx -> pure $ ctx { relationVariables = M.empty })
                          }
  ]
                                
--the precompiled functions are special because they cannot be serialized. Their names are therefore used in perpetuity so that the functions can be "serialized" (by name).
precompiledDatabaseContextFunctions :: DatabaseContextFunctions
precompiledDatabaseContextFunctions = HS.filter (not . isScriptedDatabaseContextFunction) basicDatabaseContextFunctions
                                
isScriptedDatabaseContextFunction :: DatabaseContextFunction -> Bool
isScriptedDatabaseContextFunction func = case dbcFuncBody func of
  DatabaseContextFunctionBody (Just _) _ -> True
  DatabaseContextFunctionBody Nothing _ -> False
  
databaseContextFunctionScript :: DatabaseContextFunction -> Maybe DatabaseContextFunctionBodyScript
databaseContextFunctionScript func = case dbcFuncBody func of
  DatabaseContextFunctionBody script _ -> script
  
databaseContextFunctionReturnType :: TypeConstructor -> TypeConstructor
databaseContextFunctionReturnType tCons = ADTypeConstructor "Either" [
  ADTypeConstructor "DatabaseContextFunctionError" [],
  tCons]
                                          
createScriptedDatabaseContextFunction :: DatabaseContextFunctionName -> [TypeConstructor] -> TypeConstructor -> DatabaseContextFunctionBodyScript -> DatabaseContextIOExpr
createScriptedDatabaseContextFunction funcName argsIn retArg = AddDatabaseContextFunction funcName (argsIn ++ [databaseContextFunctionReturnType retArg])

loadDatabaseContextFunctions :: ModName -> FuncName -> FilePath -> IO (Either LoadSymbolError [DatabaseContextFunction])
#ifdef PM36_HASKELL_SCRIPTING
loadDatabaseContextFunctions = loadFunction LoadAutoObjectFile
#else
loadDatabaseContextFunctions _ _ _ = pure (Left LoadSymbolError)
#endif

databaseContextFunctionsAsRelation :: DatabaseContextFunctions -> Either RelationalError Relation
databaseContextFunctionsAsRelation dbcFuncs = mkRelationFromList attrs tups
  where
    attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "arguments" TextAtomType]
    tups = map dbcFuncToTuple (HS.toList dbcFuncs)
    dbcFuncToTuple func = [TextAtom (dbcFuncName func),
                           TextAtom (dbcTextType (dbcFuncType func))]
    dbcTextType typ = T.intercalate " -> " (map prettyAtomType typ ++ ["DatabaseContext", "DatabaseContext"])

-- for merkle hash                       
hashBytes :: DatabaseContextFunction -> BL.ByteString
hashBytes func = BL.fromChunks [fname, ftype, fbody]
  where
    fname = serialise (dbcFuncName func)
    ftype = serialise (dbcFuncType func)
    fbody = case dbcFuncBody func of
      DatabaseContextFunctionBody mBody _ -> serialise mBody
