{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
module ProjectM36.AttributeExpr where
import ProjectM36.Base
import Graph.Trace
import ProjectM36.Attribute as A

attributeName :: AttributeExprBase a -> AttributeName
attributeName (AttributeAndTypeNameExpr nam _ _) = nam
attributeName (NakedAttributeExpr attr) = A.attributeName attr