{-# LANGUAGE OverloadedStrings #-}

module Backends.Python where

import qualified Data.Text as T
import Schema.Grammar

fromSchema :: Schema -> T.Text
fromSchema s = T.unlines [moduleHeader, fromSchemaWithoutHeader s]

fromSchemaWithoutHeader :: Schema -> T.Text
fromSchemaWithoutHeader (Schema tls) = T.unlines $ moduleImports : map fromTopLevelStatement tls

moduleHeader :: T.Text
moduleHeader = "Autogenerated file, do not edit."

moduleImports :: T.Text
moduleImports = T.unlines ["import dataclasses", "import typing"]

fromTopLevelStatement :: TopLevelStatement -> T.Text
fromTopLevelStatement (MessageStatement (TopLevelIdentifier name) fields) =
  T.unlines $
    [ "@dataclasses.dataclass",
      T.concat ["class ", name, ":"]
    ]
      ++ map fromFieldStatement fields

fromFieldStatement :: FieldStatement -> T.Text
fromFieldStatement (FieldStatement fieldType (FieldIdentifier name) fieldId) =
  T.concat ["  ", name, ": ", fromFieldType fieldType]

fromFieldType :: TypeExpr -> T.Text
fromFieldType StringType = "str"
fromFieldType Int32Type = "int"
fromFieldType BoolType = "bool"
fromFieldType (OptionalType t) = T.concat ["typing.Optional[", fromFieldType t, "]"]
fromFieldType (ListType t) = T.concat ["typing.List[", fromFieldType t, "]"]