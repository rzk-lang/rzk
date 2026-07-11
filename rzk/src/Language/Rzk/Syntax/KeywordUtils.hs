module Language.Rzk.Syntax.KeywordUtils where

import           Language.Rzk.Syntax.Abs

kwComma :: BNFC'Position -> CommaKeyword
kwComma = CommaKw

kwColon :: BNFC'Position -> ColonKeyword
kwColon = ColonKw

kwAssign :: BNFC'Position -> AssignKeyword
kwAssign = AssignKw

kwIn :: BNFC'Position -> InKeyword
kwIn = InKw

kwAs :: BNFC'Position -> AsKeyword
kwAs = AsKw

kwEq :: BNFC'Position -> EqKeyword
kwEq = EqKw

kwPipe :: BNFC'Position -> PipeKeyword
kwPipe = PipeKw

kwMod :: BNFC'Position -> ModKeyword
kwMod = ModKw

kwArrow :: BNFC'Position -> ArrowKeyword
kwArrow = ArrowUnicodeKw

kwMapsto :: BNFC'Position -> MapstoKeyword
kwMapsto = MapstoUnicodeKw

kwIdEqOpen :: BNFC'Position -> IdEqOpenKeyword
kwIdEqOpen = IdEqOpenKw

kwIdEqClose :: BNFC'Position -> IdEqCloseKeyword
kwIdEqClose = IdEqCloseKw

kwTopeEq :: BNFC'Position -> TopeEqKeyword
kwTopeEq = TopeEqUnicodeKw

kwTopeLeq :: BNFC'Position -> TopeLeqKeyword
kwTopeLeq = TopeLeqUnicodeKw

kwTopeAnd :: BNFC'Position -> TopeAndKeyword
kwTopeAnd = TopeAndUnicodeKw

kwTopeOr :: BNFC'Position -> TopeOrKeyword
kwTopeOr = TopeOrUnicodeKw
