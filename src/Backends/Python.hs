{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backends.Python where

import Control.Monad.Writer.Strict
import qualified Data.Text as T
import Schema.Grammar

type LineWriter m = MonadWriter [T.Text] m

runLineWriter :: Writer [T.Text] () -> T.Text
runLineWriter = T.unlines . execWriter

write :: LineWriter m => T.Text -> m ()
write l = tell [l]

indent :: LineWriter m => m a -> m a
indent = censor (map ("  " <>))

blank :: LineWriter m => m ()
blank = write ""

fromSchema :: LineWriter m => Schema -> m ()
fromSchema s = do
  moduleHeader
  fromSchemaWithoutHeader s

fromSchemaWithoutHeader :: LineWriter m => Schema -> m ()
fromSchemaWithoutHeader (Schema tls) = do
  moduleImports
  mapM_ fromTopLevelStatement tls

moduleHeader :: LineWriter m => m ()
moduleHeader = write "Autogenerated file, do not edit."

moduleImports :: LineWriter m => m ()
moduleImports = do
  write "import dataclasses"
  write "import typing"

fromTopLevelStatement :: LineWriter m => TopLevelStatement -> m ()
fromTopLevelStatement (MessageStatement (TopLevelIdentifier name) fields) = do
  blank
  write "@dataclasses.dataclass"
  write $ "class " <> name <> ":"
  indent $ do
    mapM_ fromFieldStatement fields

fromFieldStatement :: LineWriter m => FieldStatement -> m ()
fromFieldStatement (FieldStatement fieldType (FieldIdentifier name) _) =
  write $ name <> ": " <> fromFieldType fieldType

fromFieldType :: TypeExpr -> T.Text
fromFieldType StringType = "str"
fromFieldType Int32Type = "int"
fromFieldType BoolType = "bool"
fromFieldType (OptionalType t) = T.concat ["typing.Optional[", fromFieldType t, "]"]
fromFieldType (ListType t) = T.concat ["typing.List[", fromFieldType t, "]"]