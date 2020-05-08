{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Hint.Foo where

import Hint.Type
import GHC.Hs.Decls
import GHC.Hs
import SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Data.Generics.Uniplate.Operations
import OccName

data WarnFoo = WarnFoo
    { newDecl :: LHsDecl GhcPs
    }

fooHint :: DeclHint'
fooHint _ _ old
  | Just WarnFoo{newDecl} <- fooField old
  = [(suggestN' "Consider not using data Foo, maybe FooBar?" old newDecl)]
fooHint _ _ _ = []

-- data Foo = ...
fooField :: LHsDecl GhcPs -> Maybe WarnFoo
fooField (L loc (TyClD ext decl@(DataDecl _ name  _ _ _)))
    | typeNameIsFoo name =
        Just WarnFoo
              { newDecl = L loc $ TyClD ext decl {tcdLName = toTcdLName "Bar" name}
              }
fooField _ = Nothing


typeNameIsFoo name = unsafePrettyPrint name == "Foo"

-- toTcdLName :: String -> Located (IdP GhcPs)
toTcdLName newName originalDecl = transformBi replace originalDecl
  where
    replace :: OccName -> OccName
    replace (unsafePrettyPrint -> name) = mkOccName srcDataName "Bar"
