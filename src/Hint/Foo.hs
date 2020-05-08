module Hint.Foo where

import Hint.Type (Idea, DeclHint', Note(DecreasesLaziness), ideaNote, ignoreNoSuggestion', suggestN')

import Data.List (isSuffixOf)
import GHC.Hs.Decls
import GHC.Hs
import Outputable
import SrcLoc

fooHint :: DeclHint'
fooHint _ _ old
  | Just WarnNewtype{newDecl, insideType} <- fooField old
  = [(suggestN' "Consider not using data Foo" old newDecl)
        {ideaNote = [DecreasesLaziness | warnBang insideType]}]
fooHint _ _ _ = []

-- data Foo = ...
fooField :: LHsDecl GhcPs -> Maybe WarnNewtype
fooField (L loc (TyClD ext decl@(DataDecl _ _ _ _ _)))
    | Just inType <- simpleCons constructor =
        Just WarnNewtype
              { newDecl = L loc $ TyClD ext decl {tcdDataDefn = dataDef
                  { dd_ND = NewType
                  , dd_cons = map (\(L consloc x) -> L consloc $ dropConsBang x) $ dd_cons dataDef
                  }}
              , insideType = inType
              }
fooField _ = Nothing
