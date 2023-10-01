{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rzk.VSCode.Tokenize where

import           Language.LSP.Protocol.Types (SemanticTokenAbsolute (..),
                                              SemanticTokenModifiers (..),
                                              SemanticTokenTypes (..))
import           Language.Rzk.Syntax

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

  CommandPostulate _loc name _declUsedVars params type_ -> concat
    [ mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , tokenizeTerm type_
    ]
  CommandDefine _loc name _declUsedVars params type_ term -> concat
    [ mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , foldMap tokenizeTerm [type_, term]
    ]

  CommandAssume _loc vars type_ -> concat
    [ foldMap (\var -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]) vars
    , tokenizeTerm type_
    ]
  CommandSection    _loc _nameStart -> []
  CommandSectionEnd _loc _nameEnd -> []

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
  ParamPatternShapeDeprecated _loc pat cube tope -> concat
    [ tokenizePattern pat
    , tokenizeTerm cube
    , tokenizeTope tope ]

tokenizePattern :: Pattern -> [SemanticTokenAbsolute]
tokenizePattern = \case
  PatternVar _loc var    -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]
  PatternPair _loc l r   -> foldMap tokenizePattern [l, r]
  pat@(PatternUnit _loc) -> mkToken pat SemanticTokenTypes_EnumMember [SemanticTokenModifiers_Declaration]

tokenizeTope :: Term -> [SemanticTokenAbsolute]
tokenizeTope = tokenizeTerm' (Just SemanticTokenTypes_String)

tokenizeTerm :: Term -> [SemanticTokenAbsolute]
tokenizeTerm = tokenizeTerm' Nothing

tokenizeTerm' :: Maybe SemanticTokenTypes -> Term -> [SemanticTokenAbsolute]
tokenizeTerm' varTokenType = go
  where
    go term = case term of
      Hole{} -> [] -- FIXME
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

      CubeProduct _loc l r -> foldMap go [l, r]

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
      ASCII_TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      TypeId _loc x a y -> foldMap go [x, a, y]
      TypeIdSimple _loc x y -> foldMap go [x, y]

      TypeRestricted _loc type_ rs -> concat
        [ go type_
        , foldMap tokenizeRestriction rs ]

      App _loc f x -> foldMap go [f, x]
      Lambda _loc params body -> concat
        [ foldMap tokenizeParam params
        , go body ]
      ASCII_Lambda loc params body -> go (Lambda loc params body)

      Pair _loc l r -> foldMap go [l, r]
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

      RecOrDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      ASCII_TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]

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
  ParamTermTypeDeprecated _loc pat type_ -> concat
    [ tokenizePattern pat
    , tokenizeTerm type_ ]
  ParamVarShapeDeprecated _loc pat cube tope -> concat
    [ tokenizePattern pat
    , tokenizeTerm cube
    , tokenizeTope tope
    ]

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
