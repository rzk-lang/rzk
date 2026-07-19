{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rzk.VSCode.Tokenize where

import           Data.Char                   (isAlphaNum)
import           Data.List                   (sortOn)
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (SemanticTokenAbsolute (..),
                                              SemanticTokenModifiers (..),
                                              SemanticTokenTypes (..))
import           Language.Rzk.Syntax
import           Language.Rzk.Syntax.Lex     (Posn (Pn),
                                              Tok (T_HoleIdentToken, TK),
                                              TokSymbol (TokSymbol),
                                              Token (PT))
import qualified Language.Rzk.Syntax.Lex     as Lex

tokenizeModule :: Module -> [SemanticTokenAbsolute]
tokenizeModule (Module _loc langDecl commands) = concat
  [ tokenizeLanguageDecl langDecl
  , foldMap tokenizeCommand commands
  ]

tokenizeLanguageDecl :: LanguageDecl -> [SemanticTokenAbsolute]
tokenizeLanguageDecl _ = []

tokenizeCommand :: Command -> [SemanticTokenAbsolute]
tokenizeCommand command = case command of
  CommandSetOption{}   -> []    -- NOTE: fallback to TextMate
  CommandUnsetOption{} -> []    -- NOTE: fallback to TextMate
  CommandCheck        _loc term type_ -> foldMap tokenizeTerm [term, type_]
  CommandCompute      _loc term -> tokenizeTerm term
  CommandComputeNF    _loc term -> tokenizeTerm term
  CommandComputeWHNF  _loc term -> tokenizeTerm term

  CommandPostulate _loc name declUsedVars params type_ -> concat
    [ mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , tokenizeDeclUsedVars declUsedVars
    , foldMap tokenizeParam params
    , tokenizeTerm type_
    ]
  CommandDefine _loc name declUsedVars params type_ term -> concat
    [ mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , tokenizeDeclUsedVars declUsedVars
    , foldMap tokenizeParam params
    , foldMap tokenizeTerm [type_, term]
    ]

  CommandAssume _loc vars type_ -> concat
    [ foldMap (\var -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]) vars
    , tokenizeTerm type_
    ]
  CommandSection    _loc name -> tokenizeSectionName name
  CommandSectionEnd _loc name -> tokenizeSectionName name

  CommandData _loc name declUsedVars params sort body -> concat
    [ mkToken name SemanticTokenTypes_Class [SemanticTokenModifiers_Declaration]
    , tokenizeDeclUsedVars declUsedVars
    , foldMap tokenizeParam params
    , tokenizeDataSort sort
    , tokenizeDataBody body
    ]

tokenizeDataSort :: DataSort -> [SemanticTokenAbsolute]
tokenizeDataSort = \case
  SomeDataSort _loc ty -> tokenizeTerm ty
  NoDataSort _loc      -> []

tokenizeDataBody :: DataBody -> [SemanticTokenAbsolute]
tokenizeDataBody = \case
  NoDataBody _loc -> []
  SomeDataBody _loc cons elims -> concat
    [ foldMap tokenizeConstructor cons
    , foldMap tokenizeDataElim elims
    ]

tokenizeConstructor :: Constructor -> [SemanticTokenAbsolute]
tokenizeConstructor (Constructor _loc name params ctype) = concat
  [ mkToken name SemanticTokenTypes_EnumMember [SemanticTokenModifiers_Declaration]
  , foldMap tokenizeParam params
  , case ctype of
      SomeConstructorType _loc' ty -> tokenizeTerm ty
      NoConstructorType _loc'      -> []
  ]

tokenizeDataElim :: DataElim -> [SemanticTokenAbsolute]
tokenizeDataElim (DataElim _loc name ty) = concat
  [ mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
  , tokenizeTerm ty
  ]

tokenizeDeclUsedVars :: DeclUsedVars -> [SemanticTokenAbsolute]
tokenizeDeclUsedVars (DeclUsedVars _loc vars) =
  foldMap (\var -> mkToken var SemanticTokenTypes_Parameter []) vars

tokenizeSectionName :: SectionName -> [SemanticTokenAbsolute]
tokenizeSectionName = \case
  NoSectionName{}       -> []
  SomeSectionName _ name -> mkToken name SemanticTokenTypes_Property []

tokenizeBind :: Bind -> [SemanticTokenAbsolute]
tokenizeBind = \case
  BindPattern _loc pat -> tokenizePattern pat
  BindPatternType _loc pat type_ -> concat [tokenizePattern pat, tokenizeTerm type_]

tokenizeParam :: Param -> [SemanticTokenAbsolute]
tokenizeParam = \case
  ParamPattern _loc pat -> tokenizePattern pat
  ParamPatternType _loc pats type_ -> concat
    [ foldMap tokenizePattern pats
    , tokenizeTerm type_ ]
  ParamPatternShape _loc pats cube tope -> concat
    [ foldMap tokenizePattern pats
    , tokenizeTerm cube
    , tokenizeTope tope ]
  ParamPatternModalType _loc pats mc ty -> concat
    [ foldMap tokenizePattern pats
    , tokenizeModalColon mc
    , tokenizeTerm ty ]
  ParamPatternModalShape _loc pats mc cube tope -> concat
    [ foldMap tokenizePattern pats
    , tokenizeModalColon mc
    , tokenizeTerm cube
    , tokenizeTope tope ]

tokenizePattern :: Pattern -> [SemanticTokenAbsolute]
tokenizePattern = \case
  PatternVar _loc var    -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]
  PatternPair _loc l r   -> foldMap tokenizePattern [l, r]
  pat@(PatternUnit _loc) -> mkToken pat SemanticTokenTypes_EnumMember [SemanticTokenModifiers_Declaration]
  PatternTuple _loc p1 p2 ps -> foldMap tokenizePattern (p1 : p2 : ps)

tokenizeTope :: Term -> [SemanticTokenAbsolute]
tokenizeTope = tokenizeTerm' (Just SemanticTokenTypes_String)

tokenizeTerm :: Term -> [SemanticTokenAbsolute]
tokenizeTerm = tokenizeTerm' Nothing

tokenizeTerm' :: Maybe SemanticTokenTypes -> Term -> [SemanticTokenAbsolute]
tokenizeTerm' varTokenType = go
  where
    go term = case term of
      Hole{} -> [] -- highlighted from the token stream ('tokenizeSyntaxSymbols')
      Var{} -> case varTokenType of
                 Nothing         -> []
                 Just token_type -> mkToken term token_type []

      Universe{}           -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
      UniverseCube{}       -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
      UniverseTope{}       -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]

      CubeUnit{}           -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      CubeUnitStar{}       -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_CubeUnitStar{} -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      Cube2{}              -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      Cube2_0{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_Cube2_0{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      Cube2_1{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_Cube2_1{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      CubeI{}              -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      CubeI_0{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_CubeI_0{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      CubeI_1{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_CubeI_1{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_CubeI{}        -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]

      CubeProduct _loc l r -> foldMap go [l, r]
      CubeSup _loc l r -> foldMap go [l, r]
      CubeInf _loc l r -> foldMap go [l, r]

      TopeTop{}            -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeTop{}            -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeBottom{}         -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeBottom{}         -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]
      TopeInv _loc t       -> foldMap tokenizeTope [t]
      TopeUninv _loc t     -> foldMap tokenizeTope [t]
      CubeFlip _loc c      -> foldMap go [c]
      CubeUnflip _loc c    -> foldMap go [c]

      RecBottom{}          -> mkToken term SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
      RecOr _loc rs -> foldMap tokenizeRestriction rs

      TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      ASCII_TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      TypeSigmaModal loc pat mc a b -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , tokenizeModalColon mc
        , foldMap go [a, b] ]
      ASCII_TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      TypeSigmaTuple loc p ps tN -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , foldMap tokenizeSigmaParam (p : ps)
        , go tN ]
      ASCII_TypeSigmaTuple loc p ps tN -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , foldMap tokenizeSigmaParam (p : ps)
        , go tN ]
      TypeId _loc x a y -> foldMap go [x, a, y]
      TypeIdSimple _loc x y -> foldMap go [x, y]
      TypeRestricted _loc type_ rs -> concat
        [ go type_
        , foldMap tokenizeRestriction rs ]

      App _loc f x -> foldMap go [f, x]
      Lambda _loc params body -> concat
        [ foldMap tokenizeParam params
        , go body ]
      Let _loc bind val expr -> concat [tokenizeBind bind, go val, go expr]
      LetMod _loc comp bind val expr -> concat [tokenizeModComp comp, tokenizeBind bind, go val, go expr]
      ASCII_Lambda loc params body -> go (Lambda loc params body)

      Pair _loc l r -> foldMap go [l, r]
      Tuple _loc p1 p2 ps -> foldMap go (p1:p2:ps)
      First loc t -> concat
        [ mkToken (VarIdent loc "π₁") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      ASCII_First loc t -> concat
        [ mkToken (VarIdent loc "first") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      Second loc t -> concat
        [ mkToken (VarIdent loc "π₂") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      ASCII_Second loc t -> concat
        [ mkToken (VarIdent loc "second") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]

      TypeUnit _loc -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      Unit _loc -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      Refl{} -> mkToken term SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
      ReflTerm loc x -> concat
        [ mkToken (VarIdent loc "refl") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go x ]
      ReflTermType loc x a -> concat
        [ mkToken (VarIdent loc "refl") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , foldMap go [x, a] ]

      IdJ loc a b c d e f -> concat
        [ mkToken (VarIdent loc "J") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , foldMap go [a, b, c, d, e, f] ]

      TypeAsc _loc t type_ -> foldMap go [t, type_]

      ModType _loc md type_ -> concat [tokenizeModality md, go type_]
      ModApp _loc md te -> concat [tokenizeModality md, go te]
      ModExtract _loc comp te -> concat [tokenizeModComp comp, go te]


tokenizeRestriction :: Restriction -> [SemanticTokenAbsolute]
tokenizeRestriction (Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]
tokenizeRestriction (ASCII_Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]

tokenizeParamDecl :: ParamDecl -> [SemanticTokenAbsolute]
tokenizeParamDecl = \case
  ParamType _loc type_ -> tokenizeTerm type_
  ParamTermType _loc pat type_ -> concat
    [ tokenizeTerm pat
    , tokenizeTerm type_ ]
  ParamTermShape _loc pat cube tope -> concat
    [ tokenizeTerm pat
    , tokenizeTerm cube
    , tokenizeTope tope
    ]
  ParamTermModalType _loc pat mc type_ -> concat
    [ tokenizeTerm pat, tokenizeModalColon mc, tokenizeTerm type_ ]
  ParamTermModalShape _loc pat mc cube tope -> concat
    [ tokenizeTerm pat, tokenizeModalColon mc, tokenizeTerm cube, tokenizeTope tope ]

tokenizeModalColon :: ModalColon -> [SemanticTokenAbsolute]
tokenizeModalColon mc = mkToken mc SemanticTokenTypes_Decorator []

tokenizeModality :: Modality -> [SemanticTokenAbsolute]
tokenizeModality md = mkToken md SemanticTokenTypes_Decorator []

tokenizeModComp :: ModComp -> [SemanticTokenAbsolute]
tokenizeModComp = \case
  Single _loc md -> tokenizeModality md
  Comp _loc app inn -> tokenizeModality app <> tokenizeModality inn

tokenizeSigmaParam :: SigmaParam -> [SemanticTokenAbsolute]
tokenizeSigmaParam = \case
  SigmaParam _loc pat type_ -> concat
    [ tokenizePattern pat
    , tokenizeTerm type_ ]
  SigmaParamModal _loc pat mc type_ -> concat
    [ tokenizePattern pat
    , tokenizeModalColon mc
    , tokenizeTerm type_ ]

mkToken :: (HasPosition a, Print a) => a -> SemanticTokenTypes -> [SemanticTokenModifiers] -> [SemanticTokenAbsolute]
mkToken x tokenType tokenModifiers =
  case hasPosition x of
    Nothing -> []
    Just (line, col) -> do
      [ SemanticTokenAbsolute
        { _tokenType = tokenType
        , _tokenModifiers = tokenModifiers
        , _startChar = fromIntegral col - 1    -- NOTE: 0-indexed output for VS Code
        ,  _line = fromIntegral line - 1             -- NOTE: 0-indexed output for VS Code
        ,  _length = fromIntegral $ Prelude.length (printTree x)
        }
        ]

-- * Syntax highlighting from the token stream

-- | Highlight the fixed syntax of the language (command names, reserved
-- words, operators) and holes directly from the lexer token stream.
--
-- This complements 'tokenizeModule', which highlights identifiers and
-- special term formers from the parsed module. Fixed symbols do not need
-- parsing at all: a @:=@ is a @:=@ wherever it occurs, and a hole @?@ (or
-- @?name@) is its own lexer token. Working on the token stream means that
-- the grammar (and the abstract syntax) does not have to track positions of
-- keywords, and that highlighting keeps working for files that
-- (temporarily) fail to parse.
tokenizeSyntaxSymbols :: T.Text -> [SemanticTokenAbsolute]
tokenizeSyntaxSymbols input =
  [ SemanticTokenAbsolute
      { _tokenType = tokenType
      , _tokenModifiers = []
      , _line = fromIntegral line - 1      -- NOTE: 0-indexed output for LSP
      , _startChar = fromIntegral col - 1  -- NOTE: 0-indexed output for LSP
      , _length = fromIntegral (T.length sym)
      }
  | PT (Pn _ line col) tok <- Lex.tokens (tryExtractMarkdownCodeBlocks "rzk" input)
  , Just (sym, tokenType) <- [classifyToken tok]
  ]

-- | How to highlight a lexer token, if at all: fixed symbols by
-- 'classifySymbol', holes as a distinct token. Identifiers are left to the
-- AST pass ('tokenizeModule'), which knows their role.
classifyToken :: Tok -> Maybe (T.Text, SemanticTokenTypes)
classifyToken = \case
  TK (TokSymbol sym _) -> (,) sym <$> classifySymbol sym
  -- Holes are goals to come back to, so they should stand out; among the
  -- standard token types, regexp is rendered most distinctly by the default
  -- themes (themes and clients may restyle it).
  T_HoleIdentToken sym -> Just (sym, SemanticTokenTypes_Regexp)
  _                    -> Nothing

-- | How to highlight a fixed symbol of the grammar, if at all.
classifySymbol :: T.Text -> Maybe SemanticTokenTypes
classifySymbol s
  | s `elem` ignored     = Nothing
  | "#" `T.isPrefixOf` s = Just SemanticTokenTypes_Macro
  | s == "rzk-1"         = Just SemanticTokenTypes_Macro
  | T.any isAlphaNum s   = Just SemanticTokenTypes_Keyword
  | otherwise            = Just SemanticTokenTypes_Operator
  where
    -- Plain brackets are left to the editor (e.g. bracket pair colorization).
    -- @}@ is not among them: since the brace shape-parameters were removed,
    -- it occurs only as the closer of @=_{@ and @refl_{@, which classify as
    -- operators, so it takes the same colour as its opener.
    ignored = ["(", ")", "[", "]", "{", ";", "<", ">"]

-- | Combine tokens from the parsed module with tokens from the raw symbol
-- stream. On overlap (same start position) the AST-based token wins, since
-- it carries more precise semantics (e.g. @unit@ as an enum member rather
-- than a keyword). The result is sorted by position, as required for the
-- LSP delta encoding.
mergeTokens :: [SemanticTokenAbsolute] -> [SemanticTokenAbsolute] -> [SemanticTokenAbsolute]
mergeTokens astTokens symbolTokens = go (sortOn key astTokens) (sortOn key symbolTokens)
  where
    key t = (_line t, _startChar t)
    go [] ss = ss
    go as [] = as
    go (a:as) (s:ss) = case compare (key a) (key s) of
      LT -> a : go as (s:ss)
      EQ -> a : go as ss
      GT -> s : go (a:as) ss
