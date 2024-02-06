{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
{-# LANGUAGE ScopedTypeVariables #-}
--tools to execute an atom function body
module ProjectM36.AtomFunctionBody where
import Graph.Trace
import ProjectM36.Base

compiledAtomFunctionBody :: AtomFunctionBodyType -> AtomFunctionBody  
compiledAtomFunctionBody = FunctionBuiltInBody
