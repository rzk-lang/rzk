{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Rzk.VSCode.Tokenize where

import           Language.Rzk.Syntax
import           Language.Rzk.Syntax.Print
import           Language.Rzk.VSCode.Tokens

tokenizeModule :: Module -> [VSToken]
tokenizeModule (Module _loc langDecl commands) = concat
  [ tokenizeLanguageDecl langDecl
  , foldMap tokenizeCommand commands
  ]

tokenizeLanguageDecl :: LanguageDecl -> [VSToken]
tokenizeLanguageDecl _ = []

tokenizeCommand :: Command -> [VSToken]
tokenizeCommand command = case command of
  CommandSetOption{}   -> []    -- NOTE: fallback to TextMate
  CommandUnsetOption{} -> []    -- NOTE: fallback to TextMate
  CommandCheck        _loc term type_ -> foldMap tokenizeTerm [term, type_]
  CommandCompute      _loc term -> tokenizeTerm term
  CommandComputeNF    _loc term -> tokenizeTerm term
  CommandComputeWHNF  _loc term -> tokenizeTerm term

  CommandPostulate _loc name _declUsedVars params type_ -> concat
    [ mkToken name vs_function [vs_declaration]
    , foldMap tokenizeParam params
    , tokenizeTerm type_
    ]
  CommandDefine _loc name _declUsedVars params type_ term -> concat
    [ mkToken name vs_function [vs_declaration]
    , foldMap tokenizeParam params
    , foldMap tokenizeTerm [type_, term]
    ]

  CommandAssume _loc vars type_ -> concat
    [ foldMap (\var -> mkToken var vs_parameter [vs_declaration]) vars
    , tokenizeTerm type_
    ]
  CommandSection _loc _nameStart commands _nameEnd ->
    foldMap tokenizeCommand commands

tokenizeParam :: Param -> [VSToken]
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

tokenizePattern :: Pattern -> [VSToken]
tokenizePattern = \case
  PatternVar _loc var    -> mkToken var vs_parameter [vs_declaration]
  PatternPair _loc l r   -> foldMap tokenizePattern [l, r]
  pat@(PatternUnit _loc) -> mkToken pat vs_enumMember [vs_declaration]

tokenizeTope :: Term -> [VSToken]
tokenizeTope = tokenizeTerm' (Just vs_string)

tokenizeTerm :: Term -> [VSToken]
tokenizeTerm = tokenizeTerm' Nothing

tokenizeTerm' :: Maybe VSTokenType -> Term -> [VSToken]
tokenizeTerm' varTokenType = go
  where
    go term = case term of
      Hole{} -> [] -- FIXME
      Var{} -> case varTokenType of
                 Nothing         -> []
                 Just token_type -> mkToken term token_type []

      Universe{}           -> mkToken term vs_class [vs_defaultLibrary]
      UniverseCube{}       -> mkToken term vs_class [vs_defaultLibrary]
      UniverseTope{}       -> mkToken term vs_class [vs_defaultLibrary]

      CubeUnit{}           -> mkToken term vs_enum [vs_defaultLibrary]
      CubeUnitStar{}       -> mkToken term vs_enumMember [vs_defaultLibrary]
      ASCII_CubeUnitStar{} -> mkToken term vs_enumMember [vs_defaultLibrary]

      Cube2{}              -> mkToken term vs_enum [vs_defaultLibrary]
      Cube2_0{}            -> mkToken term vs_enumMember [vs_defaultLibrary]
      ASCII_Cube2_0{}      -> mkToken term vs_enumMember [vs_defaultLibrary]
      Cube2_1{}            -> mkToken term vs_enumMember [vs_defaultLibrary]
      ASCII_Cube2_1{}      -> mkToken term vs_enumMember [vs_defaultLibrary]

      CubeProduct _loc l r -> foldMap go [l, r]

      TopeTop{}            -> mkToken term vs_string [vs_defaultLibrary]
      ASCII_TopeTop{}            -> mkToken term vs_string [vs_defaultLibrary]
      TopeBottom{}         -> mkToken term vs_string [vs_defaultLibrary]
      ASCII_TopeBottom{}         -> mkToken term vs_string [vs_defaultLibrary]
      TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]

      RecBottom{}          -> mkToken term vs_function [vs_defaultLibrary]
      RecOr _loc rs -> foldMap tokenizeRestriction rs

      TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      ASCII_TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "∑") vs_class [vs_defaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      ASCII_TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "Sigma") vs_class [vs_defaultLibrary]
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
        [ mkToken (VarIdent loc "π₁") vs_function [vs_defaultLibrary]
        , go t ]
      ASCII_First loc t -> concat
        [ mkToken (VarIdent loc "first") vs_function [vs_defaultLibrary]
        , go t ]
      Second loc t -> concat
        [ mkToken (VarIdent loc "π₂") vs_function [vs_defaultLibrary]
        , go t ]
      ASCII_Second loc t -> concat
        [ mkToken (VarIdent loc "second") vs_function [vs_defaultLibrary]
        , go t ]

      TypeUnit _loc -> mkToken term vs_enum [vs_defaultLibrary]
      Unit _loc -> mkToken term vs_enumMember [vs_defaultLibrary]

      Refl{} -> mkToken term vs_function [vs_defaultLibrary]
      ReflTerm loc x -> concat
        [ mkToken (VarIdent loc "refl") vs_function [vs_defaultLibrary]
        , go x ]
      ReflTermType loc x a -> concat
        [ mkToken (VarIdent loc "refl") vs_function [vs_defaultLibrary]
        , foldMap go [x, a] ]

      IdJ loc a b c d e f -> concat
        [ mkToken (VarIdent loc "J") vs_function [vs_defaultLibrary]
        , foldMap go [a, b, c, d, e, f] ]

      TypeAsc _loc t type_ -> foldMap go [t, type_]

      RecOrDeprecated{} -> mkToken term vs_regexp [vs_deprecated]
      TypeExtensionDeprecated{} -> mkToken term vs_regexp [vs_deprecated]
      ASCII_TypeExtensionDeprecated{} -> mkToken term vs_regexp [vs_deprecated]

tokenizeRestriction :: Restriction -> [VSToken]
tokenizeRestriction (Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]
tokenizeRestriction (ASCII_Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]

tokenizeParamDecl :: ParamDecl -> [VSToken]
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

mkToken :: (HasPosition a, Print a) => a -> VSTokenType -> [VSTokenModifier] -> [VSToken]
mkToken x tokenType tokenModifiers =
  case hasPosition x of
    Nothing -> []
    Just (line, col) -> do
      [ VSToken
        { startCharacter = col - 1    -- NOTE: 0-indexed output for VS Code
        , line = line - 1             -- NOTE: 0-indexed output for VS Code
        , length = Prelude.length (printTree x)
        , .. } ]
