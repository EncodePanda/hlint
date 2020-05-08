{-# LANGUAGE NamedFieldPuns #-}
module Hint.Foo where

import Hint.Type
import GHC.Hs.Decls
import GHC.Hs
import SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

data WarnFoo = WarnFoo
    { newDecl :: LHsDecl GhcPs
    }

fooHint :: DeclHint'
fooHint _ _ old
  | Just WarnFoo{newDecl} <- fooField old
  = [(suggestN' "Consider not using data Foo" old newDecl)]
fooHint _ _ _ = []

-- data Foo = ...
fooField :: LHsDecl GhcPs -> Maybe WarnFoo
fooField (L loc (TyClD ext decl@(DataDecl _ name  _ _ _)))
    | typeNameIsFoo name =
        Just WarnFoo
              { newDecl = L loc $ TyClD ext decl
              }
fooField _ = Nothing


typeNameIsFoo name = unsafePrettyPrint name == "Foo"
