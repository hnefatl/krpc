{-# LANGUAGE OverloadedStrings #-}

module Backends.Python where

import qualified Data.Text as T
--import qualified Language.Python.Common as P
import Schema.Grammar

-- Convenience wrappers around the python AST library

--makeIdent :: String -> P.Ident ()
--makeIdent s = P.Ident s ()
--
--makeImportFrom :: [String] -> [String] -> P.Statement ()
--makeImportFrom mods items =
--  P.FromImport
--    { P.from_module =
--        P.ImportRelative
--          { P.import_relative_dots = 0,
--            P.import_relative_module = Just $ map makeIdent mods,
--            P.import_relative_annot = ()
--          },
--      P.from_items =
--        P.FromItems
--          { P.from_items_items = map (\i -> P.FromItem (makeIdent i) Nothing ()) items,
--            P.from_items_annot = ()
--          },
--      stmt_annot = ()
--    }
--
--fromSchema :: Schema -> P.Module ()
--fromSchema (Schema tls) = P.Module $ makeModuleImports ++ map fromTopLevelStatement tls
--fromTopLevelStatement :: TopLevelStatement -> P.Statement ()
--fromTopLevelStatement = undefined

fromSchema :: Schema -> T.Text
fromSchema s = T.unlines [moduleHeader, fromSchemaWithoutHeader s]

fromSchemaWithoutHeader :: Schema -> T.Text
fromSchemaWithoutHeader (Schema tls) = T.unlines $ [moduleImports] ++ map fromTopLevelStatement tls

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
  T.concat ["  ", name, ": ", fromFieldTypeOptional fieldType]

fromFieldType :: TypeExpr -> T.Text
fromFieldType StringType = "str"
fromFieldType Int32Type = "int"
fromFieldType BoolType = "bool"
fromFieldType (ListType t) = T.concat ["typing.List[", fromFieldType t, "]"]

fromFieldTypeOptional :: TypeExpr -> T.Text
fromFieldTypeOptional t@ListType {} = fromFieldType t
fromFieldTypeOptional t = optional (fromFieldType t)

optional :: T.Text -> T.Text
optional t = T.concat ["typing.Optional[", t, "]"]