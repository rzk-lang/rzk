-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Rzk.Syntax.Print where

import           Data.Char               (Char, isSpace)
import qualified Language.Rzk.Syntax.Abs
import           Prelude                 (Bool (..), Double, Int, Integer,
                                          ShowS, String, all, elem, foldr, id,
                                          map, null, replicate, showChar,
                                          showString, shows, span, ($), (*),
                                          (+), (++), (-), (.), (<), (==))

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
--      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
--      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
--      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\'       -> showString "\\\\"
  '\n'       -> showString "\\n"
  '\t'       -> showString "\\t"
  s          -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Language.Rzk.Syntax.Abs.VarIdentToken where
  prt _ (Language.Rzk.Syntax.Abs.VarIdentToken i) = doc $ showString i
instance Print Language.Rzk.Syntax.Abs.HoleIdentToken where
  prt _ (Language.Rzk.Syntax.Abs.HoleIdentToken i) = doc $ showString i
instance Print (Language.Rzk.Syntax.Abs.Module' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.Module _ languagedecl commands -> prPrec i 0 (concatD [prt 0 languagedecl, prt 0 commands])

instance Print (Language.Rzk.Syntax.Abs.HoleIdent' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.HoleIdent _ holeidenttoken -> prPrec i 0 (concatD [prt 0 holeidenttoken])

instance Print (Language.Rzk.Syntax.Abs.VarIdent' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.VarIdent _ varidenttoken -> prPrec i 0 (concatD [prt 0 varidenttoken])

instance Print [Language.Rzk.Syntax.Abs.VarIdent' a] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Language.Rzk.Syntax.Abs.LanguageDecl' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.LanguageDecl _ language -> prPrec i 0 (concatD [doc (showString "#lang"), prt 0 language, doc (showString ";")])

instance Print (Language.Rzk.Syntax.Abs.Language' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.Rzk1 _ -> prPrec i 0 (concatD [doc (showString "rzk-1")])

instance Print (Language.Rzk.Syntax.Abs.Command' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.CommandSetOption _ str1 str2 -> prPrec i 0 (concatD [doc (showString "#set-option"), printString str1, doc (showString "="), printString str2])
    Language.Rzk.Syntax.Abs.CommandUnsetOption _ str -> prPrec i 0 (concatD [doc (showString "#unset-option"), printString str])
    Language.Rzk.Syntax.Abs.CommandCheck _ term1 term2 -> prPrec i 0 (concatD [doc (showString "#check"), prt 0 term1, doc (showString ":"), prt 0 term2])
    Language.Rzk.Syntax.Abs.CommandCompute _ term -> prPrec i 0 (concatD [doc (showString "#compute"), prt 0 term])
    Language.Rzk.Syntax.Abs.CommandComputeWHNF _ term -> prPrec i 0 (concatD [doc (showString "#compute-whnf"), prt 0 term])
    Language.Rzk.Syntax.Abs.CommandComputeNF _ term -> prPrec i 0 (concatD [doc (showString "#compute-nf"), prt 0 term])
    Language.Rzk.Syntax.Abs.CommandPostulate _ varident declusedvars params term -> prPrec i 0 (concatD [doc (showString "#postulate"), prt 0 varident, prt 0 declusedvars, prt 0 params, doc (showString ":"), prt 0 term])
    Language.Rzk.Syntax.Abs.CommandAssume _ varidents term -> prPrec i 0 (concatD [doc (showString "#assume"), prt 0 varidents, doc (showString ":"), prt 0 term])
    Language.Rzk.Syntax.Abs.CommandSection _ sectionname1 commands sectionname2 -> prPrec i 0 (concatD [doc (showString "#section"), prt 0 sectionname1, doc (showString ";"), prt 0 commands, doc (showString "#end"), prt 0 sectionname2])
    Language.Rzk.Syntax.Abs.CommandDefine _ varident declusedvars params term1 term2 -> prPrec i 0 (concatD [doc (showString "#define"), prt 0 varident, prt 0 declusedvars, prt 0 params, doc (showString ":"), prt 0 term1, doc (showString ":="), prt 0 term2])

instance Print [Language.Rzk.Syntax.Abs.Command' a] where
  prt _ []     = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (Language.Rzk.Syntax.Abs.DeclUsedVars' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.DeclUsedVars _ varidents -> prPrec i 0 (concatD [doc (showString "uses"), doc (showString "("), prt 0 varidents, doc (showString ")")])

instance Print (Language.Rzk.Syntax.Abs.SectionName' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.NoSectionName _ -> prPrec i 0 (concatD [])
    Language.Rzk.Syntax.Abs.SomeSectionName _ varident -> prPrec i 0 (concatD [prt 0 varident])

instance Print (Language.Rzk.Syntax.Abs.Pattern' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.PatternUnit _ -> prPrec i 0 (concatD [doc (showString "unit")])
    Language.Rzk.Syntax.Abs.PatternVar _ varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Rzk.Syntax.Abs.PatternPair _ pattern_1 pattern_2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_1, doc (showString ","), prt 0 pattern_2, doc (showString ")")])

instance Print [Language.Rzk.Syntax.Abs.Pattern' a] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Language.Rzk.Syntax.Abs.Param' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.ParamPattern _ pattern_ -> prPrec i 0 (concatD [prt 0 pattern_])
    Language.Rzk.Syntax.Abs.ParamPatternType _ patterns term -> prPrec i 0 (concatD [doc (showString "("), prt 0 patterns, doc (showString ":"), prt 0 term, doc (showString ")")])
    Language.Rzk.Syntax.Abs.ParamPatternShape _ patterns term1 term2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 patterns, doc (showString ":"), prt 0 term1, doc (showString "|"), prt 0 term2, doc (showString ")")])
    Language.Rzk.Syntax.Abs.ParamPatternShapeDeprecated _ pattern_ term1 term2 -> prPrec i 0 (concatD [doc (showString "{"), prt 0 pattern_, doc (showString ":"), prt 0 term1, doc (showString "|"), prt 0 term2, doc (showString "}")])

instance Print [Language.Rzk.Syntax.Abs.Param' a] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Language.Rzk.Syntax.Abs.ParamDecl' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.ParamType _ term -> prPrec i 0 (concatD [prt 6 term])
    Language.Rzk.Syntax.Abs.ParamTermType _ term1 term2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 term1, doc (showString ":"), prt 0 term2, doc (showString ")")])
    Language.Rzk.Syntax.Abs.ParamTermShape _ term1 term2 term3 -> prPrec i 0 (concatD [doc (showString "("), prt 0 term1, doc (showString ":"), prt 0 term2, doc (showString "|"), prt 0 term3, doc (showString ")")])
    Language.Rzk.Syntax.Abs.ParamTermTypeDeprecated _ pattern_ term -> prPrec i 0 (concatD [doc (showString "{"), prt 0 pattern_, doc (showString ":"), prt 0 term, doc (showString "}")])
    Language.Rzk.Syntax.Abs.ParamVarShapeDeprecated _ pattern_ term1 term2 -> prPrec i 0 (concatD [doc (showString "{"), doc (showString "("), prt 0 pattern_, doc (showString ":"), prt 0 term1, doc (showString ")"), doc (showString "|"), prt 0 term2, doc (showString "}")])

instance Print (Language.Rzk.Syntax.Abs.Restriction' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.Restriction _ term1 term2 -> prPrec i 0 (concatD [prt 0 term1, doc (showString "\8614"), prt 0 term2])
    Language.Rzk.Syntax.Abs.ASCII_Restriction _ term1 term2 -> prPrec i 0 (concatD [prt 0 term1, doc (showString "|->"), prt 0 term2])

instance Print [Language.Rzk.Syntax.Abs.Restriction' a] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Language.Rzk.Syntax.Abs.Term' a) where
  prt i = \case
    Language.Rzk.Syntax.Abs.Universe _ -> prPrec i 7 (concatD [doc (showString "U")])
    Language.Rzk.Syntax.Abs.UniverseCube _ -> prPrec i 7 (concatD [doc (showString "CUBE")])
    Language.Rzk.Syntax.Abs.UniverseTope _ -> prPrec i 7 (concatD [doc (showString "TOPE")])
    Language.Rzk.Syntax.Abs.CubeUnit _ -> prPrec i 7 (concatD [doc (showString "1")])
    Language.Rzk.Syntax.Abs.CubeUnitStar _ -> prPrec i 7 (concatD [doc (showString "*\8321")])
    Language.Rzk.Syntax.Abs.Cube2 _ -> prPrec i 7 (concatD [doc (showString "2")])
    Language.Rzk.Syntax.Abs.Cube2_0 _ -> prPrec i 7 (concatD [doc (showString "0\8322")])
    Language.Rzk.Syntax.Abs.Cube2_1 _ -> prPrec i 7 (concatD [doc (showString "1\8322")])
    Language.Rzk.Syntax.Abs.CubeProduct _ term1 term2 -> prPrec i 5 (concatD [prt 5 term1, doc (showString "\215"), prt 6 term2])
    Language.Rzk.Syntax.Abs.TopeTop _ -> prPrec i 7 (concatD [doc (showString "\8868")])
    Language.Rzk.Syntax.Abs.TopeBottom _ -> prPrec i 7 (concatD [doc (showString "\8869")])
    Language.Rzk.Syntax.Abs.TopeEQ _ term1 term2 -> prPrec i 4 (concatD [prt 5 term1, doc (showString "\8801"), prt 5 term2])
    Language.Rzk.Syntax.Abs.TopeLEQ _ term1 term2 -> prPrec i 4 (concatD [prt 5 term1, doc (showString "\8804"), prt 5 term2])
    Language.Rzk.Syntax.Abs.TopeAnd _ term1 term2 -> prPrec i 3 (concatD [prt 4 term1, doc (showString "\8743"), prt 3 term2])
    Language.Rzk.Syntax.Abs.TopeOr _ term1 term2 -> prPrec i 2 (concatD [prt 3 term1, doc (showString "\8744"), prt 2 term2])
    Language.Rzk.Syntax.Abs.RecBottom _ -> prPrec i 7 (concatD [doc (showString "recBOT")])
    Language.Rzk.Syntax.Abs.RecOr _ restrictions -> prPrec i 7 (concatD [doc (showString "recOR"), doc (showString "("), prt 0 restrictions, doc (showString ")")])
    Language.Rzk.Syntax.Abs.RecOrDeprecated _ term1 term2 term3 term4 -> prPrec i 7 (concatD [doc (showString "recOR"), doc (showString "("), prt 0 term1, doc (showString ","), prt 0 term2, doc (showString ","), prt 0 term3, doc (showString ","), prt 0 term4, doc (showString ")")])
    Language.Rzk.Syntax.Abs.TypeFun _ paramdecl term -> prPrec i 1 (concatD [prt 0 paramdecl, doc (showString "\8594"), prt 1 term])
    Language.Rzk.Syntax.Abs.TypeSigma _ pattern_ term1 term2 -> prPrec i 1 (concatD [doc (showString "\931"), doc (showString "("), prt 0 pattern_, doc (showString ":"), prt 0 term1, doc (showString ")"), doc (showString ","), prt 1 term2])
    Language.Rzk.Syntax.Abs.TypeUnit _ -> prPrec i 7 (concatD [doc (showString "Unit")])
    Language.Rzk.Syntax.Abs.TypeId _ term1 term2 term3 -> prPrec i 1 (concatD [prt 2 term1, doc (showString "=_{"), prt 0 term2, doc (showString "}"), prt 2 term3])
    Language.Rzk.Syntax.Abs.TypeIdSimple _ term1 term2 -> prPrec i 1 (concatD [prt 2 term1, doc (showString "="), prt 2 term2])
    Language.Rzk.Syntax.Abs.TypeRestricted _ term restrictions -> prPrec i 6 (concatD [prt 6 term, doc (showString "["), prt 0 restrictions, doc (showString "]")])
    Language.Rzk.Syntax.Abs.TypeExtensionDeprecated _ paramdecl term -> prPrec i 7 (concatD [doc (showString "<"), prt 0 paramdecl, doc (showString "\8594"), prt 0 term, doc (showString ">")])
    Language.Rzk.Syntax.Abs.App _ term1 term2 -> prPrec i 6 (concatD [prt 6 term1, prt 7 term2])
    Language.Rzk.Syntax.Abs.Lambda _ params term -> prPrec i 1 (concatD [doc (showString "\\"), prt 0 params, doc (showString "\8594"), prt 1 term])
    Language.Rzk.Syntax.Abs.Pair _ term1 term2 -> prPrec i 7 (concatD [doc (showString "("), prt 0 term1, doc (showString ","), prt 0 term2, doc (showString ")")])
    Language.Rzk.Syntax.Abs.First _ term -> prPrec i 6 (concatD [doc (showString "\960\8321"), prt 7 term])
    Language.Rzk.Syntax.Abs.Second _ term -> prPrec i 6 (concatD [doc (showString "\960\8322"), prt 7 term])
    Language.Rzk.Syntax.Abs.Unit _ -> prPrec i 7 (concatD [doc (showString "unit")])
    Language.Rzk.Syntax.Abs.Refl _ -> prPrec i 7 (concatD [doc (showString "refl")])
    Language.Rzk.Syntax.Abs.ReflTerm _ term -> prPrec i 7 (concatD [doc (showString "refl_{"), prt 0 term, doc (showString "}")])
    Language.Rzk.Syntax.Abs.ReflTermType _ term1 term2 -> prPrec i 7 (concatD [doc (showString "refl_{"), prt 0 term1, doc (showString ":"), prt 0 term2, doc (showString "}")])
    Language.Rzk.Syntax.Abs.IdJ _ term1 term2 term3 term4 term5 term6 -> prPrec i 7 (concatD [doc (showString "idJ"), doc (showString "("), prt 0 term1, doc (showString ","), prt 0 term2, doc (showString ","), prt 0 term3, doc (showString ","), prt 0 term4, doc (showString ","), prt 0 term5, doc (showString ","), prt 0 term6, doc (showString ")")])
    Language.Rzk.Syntax.Abs.Hole _ holeident -> prPrec i 7 (concatD [prt 0 holeident])
    Language.Rzk.Syntax.Abs.Var _ varident -> prPrec i 7 (concatD [prt 0 varident])
    Language.Rzk.Syntax.Abs.TypeAsc _ term1 term2 -> prPrec i 0 (concatD [prt 2 term1, doc (showString "as"), prt 1 term2])
    Language.Rzk.Syntax.Abs.ASCII_CubeUnitStar _ -> prPrec i 7 (concatD [doc (showString "*_1")])
    Language.Rzk.Syntax.Abs.ASCII_Cube2_0 _ -> prPrec i 7 (concatD [doc (showString "0_2")])
    Language.Rzk.Syntax.Abs.ASCII_Cube2_1 _ -> prPrec i 7 (concatD [doc (showString "1_2")])
    Language.Rzk.Syntax.Abs.ASCII_TopeTop _ -> prPrec i 7 (concatD [doc (showString "TOP")])
    Language.Rzk.Syntax.Abs.ASCII_TopeBottom _ -> prPrec i 7 (concatD [doc (showString "BOT")])
    Language.Rzk.Syntax.Abs.ASCII_TopeEQ _ term1 term2 -> prPrec i 4 (concatD [prt 5 term1, doc (showString "==="), prt 5 term2])
    Language.Rzk.Syntax.Abs.ASCII_TopeLEQ _ term1 term2 -> prPrec i 4 (concatD [prt 5 term1, doc (showString "<="), prt 5 term2])
    Language.Rzk.Syntax.Abs.ASCII_TopeAnd _ term1 term2 -> prPrec i 3 (concatD [prt 4 term1, doc (showString "/\\"), prt 3 term2])
    Language.Rzk.Syntax.Abs.ASCII_TopeOr _ term1 term2 -> prPrec i 2 (concatD [prt 3 term1, doc (showString "\\/"), prt 2 term2])
    Language.Rzk.Syntax.Abs.ASCII_TypeFun _ paramdecl term -> prPrec i 1 (concatD [prt 0 paramdecl, doc (showString "->"), prt 1 term])
    Language.Rzk.Syntax.Abs.ASCII_TypeSigma _ pattern_ term1 term2 -> prPrec i 1 (concatD [doc (showString "Sigma"), doc (showString "("), prt 0 pattern_, doc (showString ":"), prt 0 term1, doc (showString ")"), doc (showString ","), prt 1 term2])
    Language.Rzk.Syntax.Abs.ASCII_Lambda _ params term -> prPrec i 1 (concatD [doc (showString "\\"), prt 0 params, doc (showString "->"), prt 1 term])
    Language.Rzk.Syntax.Abs.ASCII_TypeExtensionDeprecated _ paramdecl term -> prPrec i 7 (concatD [doc (showString "<"), prt 0 paramdecl, doc (showString "->"), prt 0 term, doc (showString ">")])
    Language.Rzk.Syntax.Abs.ASCII_First _ term -> prPrec i 6 (concatD [doc (showString "first"), prt 7 term])
    Language.Rzk.Syntax.Abs.ASCII_Second _ term -> prPrec i 6 (concatD [doc (showString "second"), prt 7 term])

instance Print [Language.Rzk.Syntax.Abs.Term' a] where
  prt _ []     = concatD []
  prt _ [x]    = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]
