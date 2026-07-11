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
tokenizeLanguageDecl (LanguageDecl loc language) = concat
  [ mkToken (VarIdent loc "#lang") SemanticTokenTypes_Macro []
  , case language of
      Rzk1 langLoc -> mkToken (VarIdent langLoc "rzk-1") SemanticTokenTypes_Macro []
  ]

tokenizeCommand :: Command -> [SemanticTokenAbsolute]
tokenizeCommand command = case command of
  CommandSetOption loc _ eq _ -> concat
    [ mkToken (VarIdent loc "#set-option") SemanticTokenTypes_Macro []
    , mkToken eq SemanticTokenTypes_Operator []
    ]
  CommandUnsetOption loc _ -> mkToken (VarIdent loc "#unset-option") SemanticTokenTypes_Macro []
  CommandCheck loc term colon type_ -> concat
    [ mkToken (VarIdent loc "#check") SemanticTokenTypes_Macro []
    , tokenizeTerm term
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandCompute loc term -> concat
    [ mkToken (VarIdent loc "#compute") SemanticTokenTypes_Macro []
    , tokenizeTerm term
    ]
  CommandComputeNF loc term -> concat
    [ mkToken (VarIdent loc "#compute-nf") SemanticTokenTypes_Macro []
    , tokenizeTerm term
    ]
  CommandComputeWHNF loc term -> concat
    [ mkToken (VarIdent loc "#compute-whnf") SemanticTokenTypes_Macro []
    , tokenizeTerm term
    ]

  CommandPostulate loc name declUsedVars params colon type_ -> concat
    [ mkToken (VarIdent loc "#postulate") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandPostulateNoParams loc name declUsedVars colon type_ -> concat
    [ mkToken (VarIdent loc "#postulate") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandDefine loc name declUsedVars params colon type_ assign term -> concat
    [ mkToken (VarIdent loc "#define") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    , mkToken assign SemanticTokenTypes_Operator []
    , tokenizeTerm term
    ]
  CommandDefineNoParams loc name declUsedVars colon type_ assign term -> concat
    [ mkToken (VarIdent loc "#define") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    , mkToken assign SemanticTokenTypes_Operator []
    , tokenizeTerm term
    ]
  CommandDef loc name declUsedVars params colon type_ assign term -> concat
    [ mkToken (VarIdent loc "#def") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    , mkToken assign SemanticTokenTypes_Operator []
    , tokenizeTerm term
    ]
  CommandDefNoParams loc name declUsedVars colon type_ assign term -> concat
    [ mkToken (VarIdent loc "#def") SemanticTokenTypes_Macro []
    , tokenizeDeclUsedVars declUsedVars
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    , mkToken assign SemanticTokenTypes_Operator []
    , tokenizeTerm term
    ]

  CommandAssume loc vars colon type_ -> concat
    [ mkToken (VarIdent loc "#assume") SemanticTokenTypes_Macro []
    , foldMap (\var -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]) vars
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandVariable loc name colon type_ -> concat
    [ mkToken (VarIdent loc "#variable") SemanticTokenTypes_Macro []
    , mkToken name SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandVariables loc vars colon type_ -> concat
    [ mkToken (VarIdent loc "#variables") SemanticTokenTypes_Macro []
    , foldMap (\var -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]) vars
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]
  CommandSection loc name -> concat
    [ mkToken (VarIdent loc "#section") SemanticTokenTypes_Macro []
    , case name of
        NoSectionName{}        -> []
        SomeSectionName _ n -> mkToken n SemanticTokenTypes_Property []
    ]
  CommandSectionEnd loc name -> concat
    [ mkToken (VarIdent loc "#end") SemanticTokenTypes_Macro []
    , case name of
        NoSectionName{}        -> []
        SomeSectionName _ n -> mkToken n SemanticTokenTypes_Property []
    ]

tokenizeDeclUsedVars :: DeclUsedVars -> [SemanticTokenAbsolute]
tokenizeDeclUsedVars = \case
  EmptyDeclUsedVars{} -> []
  DeclUsedVars _loc uses vars -> concat
    [ mkToken uses SemanticTokenTypes_Keyword []
    , foldMap (\var -> mkToken var SemanticTokenTypes_Parameter []) vars
    ]

tokenizeBind :: Bind -> [SemanticTokenAbsolute]
tokenizeBind = \case
  BindPattern _loc pat -> tokenizePattern pat
  BindPatternType _loc pat colon type_ -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_
    ]

tokenizeParam :: Param -> [SemanticTokenAbsolute]
tokenizeParam = \case
  ParamPattern _loc pat -> tokenizePattern pat
  ParamPatternType _loc pats colon type_ -> concat
    [ foldMap tokenizePattern pats
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_ ]
  ParamPatternShape _loc pats colon cube pipe tope -> concat
    [ foldMap tokenizePattern pats
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope ]
  ParamPatternShapeDeprecated _loc pat colon cube pipe tope -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope ]
  ParamPatternModalType _loc pats mc ty -> concat
    [ foldMap tokenizePattern pats
    , tokenizeModalColon mc
    , tokenizeTerm ty ]
  ParamPatternModalShape _loc pats mc cube pipe tope -> concat
    [ foldMap tokenizePattern pats
    , tokenizeModalColon mc
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope ]

tokenizePattern :: Pattern -> [SemanticTokenAbsolute]
tokenizePattern = \case
  PatternVar _loc var    -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]
  PatternPair _loc l comma r -> concat [tokenizePattern l, mkToken comma SemanticTokenTypes_Operator [], tokenizePattern r]
  pat@(PatternUnit _loc) -> mkToken pat SemanticTokenTypes_EnumMember [SemanticTokenModifiers_Declaration]
  PatternTuple _loc p1 c1 p2 c2 ps -> concat
    [ tokenizePattern p1
    , mkToken c1 SemanticTokenTypes_Operator []
    , tokenizePattern p2
    , mkToken c2 SemanticTokenTypes_Operator []
    , foldMap tokenizePattern ps
    ]

tokenizeTope :: Term -> [SemanticTokenAbsolute]
tokenizeTope = tokenizeTerm' (Just SemanticTokenTypes_String)

tokenizeTerm :: Term -> [SemanticTokenAbsolute]
tokenizeTerm = tokenizeTerm' Nothing

tokenizeTerm' :: Maybe SemanticTokenTypes -> Term -> [SemanticTokenAbsolute]
tokenizeTerm' varTokenType = go
  where
    go term = case term of
      Hole _loc hid -> mkToken hid SemanticTokenTypes_Macro []
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
      ASCII_CubeProduct _loc l r -> foldMap go [l, r]

      TopeTop{}            -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeTop{}            -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeBottom{}         -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeBottom{}         -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeAnd _loc l op r     -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      ASCII_TopeAnd _loc l op r -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      TopeOr  _loc l op r     -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      ASCII_TopeOr  _loc l op r -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      TopeEQ  _loc l op r     -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      ASCII_TopeEQ  _loc l op r -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      TopeLEQ _loc l op r     -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      ASCII_TopeLEQ _loc l op r -> concat [tokenizeTope l, mkToken op SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary], tokenizeTope r]
      TopeInv loc t        -> concat
        [ mkToken (VarIdent loc "invᵒᵖ") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeTope t ]
      ASCII_TopeInv loc t  -> concat
        [ mkToken (VarIdent loc "inv_op") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeTope t ]
      TopeUninv loc t      -> concat
        [ mkToken (VarIdent loc "uninvᵒᵖ") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeTope t ]
      ASCII_TopeUninv loc t -> concat
        [ mkToken (VarIdent loc "uninv_op") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeTope t ]
      CubeFlip loc c       -> concat
        [ mkToken (VarIdent loc "flipᵒᵖ") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go c ]
      ASCII_CubeFlip loc c -> concat
        [ mkToken (VarIdent loc "flip_op") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go c ]
      CubeUnflip loc c     -> concat
        [ mkToken (VarIdent loc "unflipᵒᵖ") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go c ]
      ASCII_CubeUnflip loc c -> concat
        [ mkToken (VarIdent loc "unflip_op") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go c ]

      RecBottom{}          -> mkToken term SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
      RecOr loc rs -> concat
        [ mkToken (VarIdent loc "recOR") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , foldMap tokenizeRestriction rs ]

      TypeFun _loc paramDecl arrow ret -> concat
        [ tokenizeParamDecl paramDecl
        , mkToken arrow SemanticTokenTypes_Operator []
        , go ret ]
      ASCII_TypeFun _loc paramDecl arrow ret -> concat
        [ tokenizeParamDecl paramDecl
        , mkToken arrow SemanticTokenTypes_Operator []
        , go ret ]
      TypeSigma loc pat colon a comma b -> concat
        [ mkToken (VarIdent loc "Σ") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , mkToken colon SemanticTokenTypes_Operator []
        , go a
        , mkToken comma SemanticTokenTypes_Operator []
        , go b ]
      TypeSigmaModal loc pat mc ty comma b -> concat
        [ mkToken (VarIdent loc "Σ") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , tokenizeModalColon mc
        , go ty
        , mkToken comma SemanticTokenTypes_Operator []
        , go b ]
      ASCII_TypeSigma loc pat colon a comma b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , mkToken colon SemanticTokenTypes_Operator []
        , go a
        , mkToken comma SemanticTokenTypes_Operator []
        , go b ]
      ASCII_TypeSigmaModal loc pat mc ty comma b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , tokenizeModalColon mc
        , go ty
        , mkToken comma SemanticTokenTypes_Operator []
        , go b ]
      Unicode_TypeSigmaAlt loc pat colon a comma b -> concat
        [ mkToken (VarIdent loc "Σ") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , mkToken colon SemanticTokenTypes_Operator []
        , go a
        , mkToken comma SemanticTokenTypes_Operator []
        , go b ]
      TypeSigmaTuple loc p comma ps comma2 tN -> concat
        [ mkToken (VarIdent loc "Σ") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeSigmaParam p
        , mkToken comma SemanticTokenTypes_Operator []
        , foldMap tokenizeSigmaParam ps
        , mkToken comma2 SemanticTokenTypes_Operator []
        , go tN ]
      ASCII_TypeSigmaTuple loc p comma ps comma2 tN -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeSigmaParam p
        , mkToken comma SemanticTokenTypes_Operator []
        , foldMap tokenizeSigmaParam ps
        , mkToken comma2 SemanticTokenTypes_Operator []
        , go tN ]
      Unicode_TypeSigmaTupleAlt loc p comma ps comma2 tN -> concat
        [ mkToken (VarIdent loc "Σ") SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
        , tokenizeSigmaParam p
        , mkToken comma SemanticTokenTypes_Operator []
        , foldMap tokenizeSigmaParam ps
        , mkToken comma2 SemanticTokenTypes_Operator []
        , go tN ]
      TypeId _loc x open idx close y -> concat
        [ go x
        , mkToken open SemanticTokenTypes_Operator []
        , go idx
        , mkToken close SemanticTokenTypes_Operator []
        , go y ]
      TypeIdSimple _loc x eq y -> concat [go x, mkToken eq SemanticTokenTypes_Operator [], go y]
      TypeRestricted _loc type_ rs -> concat
        [ go type_
        , foldMap tokenizeRestriction rs ]

      App _loc f x -> foldMap go [f, x]
      Lambda loc params arrow body -> concat
        [ mkToken (VarIdent loc "\\") SemanticTokenTypes_Operator []
        , foldMap tokenizeParam params
        , mkToken arrow SemanticTokenTypes_Operator []
        , go body ]
      Let loc bind assign val inKw expr -> concat
        [ mkToken (VarIdent loc "let") SemanticTokenTypes_Keyword []
        , tokenizeBind bind
        , mkToken assign SemanticTokenTypes_Operator []
        , go val
        , mkToken inKw SemanticTokenTypes_Keyword []
        , go expr ]
      LetMod loc modKw comp bind assign val inKw expr -> concat
        [ mkToken (VarIdent loc "let") SemanticTokenTypes_Keyword []
        , mkToken modKw SemanticTokenTypes_Keyword []
        , tokenizeModComp comp
        , tokenizeBind bind
        , mkToken assign SemanticTokenTypes_Operator []
        , go val
        , mkToken inKw SemanticTokenTypes_Keyword []
        , go expr ]
      ASCII_Lambda loc params arrow body -> go (Lambda loc params arrow body)

      Pair _loc l comma r -> concat [go l, mkToken comma SemanticTokenTypes_Operator [], go r]
      Tuple _loc p1 c1 p2 c2 ps -> concat
        [ go p1, mkToken c1 SemanticTokenTypes_Operator []
        , go p2, mkToken c2 SemanticTokenTypes_Operator []
        , foldMap go ps
        ]
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
      ReflTermType loc x colon a -> concat
        [ mkToken (VarIdent loc "refl") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go x
        , mkToken colon SemanticTokenTypes_Operator []
        , go a ]

      IdJ loc a c1 b c2 c c3 d c4 e c5 f -> concat
        [ mkToken (VarIdent loc "idJ") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go a, mkToken c1 SemanticTokenTypes_Operator []
        , go b, mkToken c2 SemanticTokenTypes_Operator []
        , go c, mkToken c3 SemanticTokenTypes_Operator []
        , go d, mkToken c4 SemanticTokenTypes_Operator []
        , go e, mkToken c5 SemanticTokenTypes_Operator []
        , go f ]

      TypeAsc _loc t asKw type_ -> concat [go t, mkToken asKw SemanticTokenTypes_Keyword [], go type_]

      ModType _loc md type_ -> concat [tokenizeModality md, go type_]
      ModApp _loc modKw md te -> concat
        [ mkToken modKw SemanticTokenTypes_Keyword []
        , tokenizeModality md
        , go te ]
      ModExtract loc comp te -> concat
        [ mkToken (VarIdent loc "$extract$") SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
        , tokenizeModComp comp
        , go te ]

      RecOrDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      ASCII_TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]


tokenizeRestriction :: Restriction -> [SemanticTokenAbsolute]
tokenizeRestriction = \case
  Restriction _loc tope mapsto term -> concat
    [ tokenizeTope tope
    , mkToken mapsto SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
    , tokenizeTerm term ]
  ASCII_Restriction _loc tope mapsto term -> concat
    [ tokenizeTope tope
    , mkToken mapsto SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
    , tokenizeTerm term ]

tokenizeParamDecl :: ParamDecl -> [SemanticTokenAbsolute]
tokenizeParamDecl = \case
  ParamType _loc type_ -> tokenizeTerm type_
  ParamTermType _loc pat colon type_ -> concat
    [ tokenizeTerm pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_ ]
  ParamTermShape _loc pat colon cube pipe tope -> concat
    [ tokenizeTerm pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope
    ]
  ParamTermTypeDeprecated _loc pat colon type_ -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm type_ ]
  ParamVarShapeDeprecated _loc pat colon cube pipe tope -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope
    ]
  ParamVarShapeDeprecatedAlt _loc pat colon cube pipe tope -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
    , tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope
    ]
  ParamTermModalType _loc pat mc type_ -> concat
    [ tokenizeTerm pat, tokenizeModalColon mc, tokenizeTerm type_ ]
  ParamTermModalShape _loc pat mc cube pipe tope -> concat
    [ tokenizeTerm pat, tokenizeModalColon mc, tokenizeTerm cube
    , mkToken pipe SemanticTokenTypes_Operator []
    , tokenizeTope tope ]

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
  SigmaParam _loc pat colon type_ -> concat
    [ tokenizePattern pat
    , mkToken colon SemanticTokenTypes_Operator []
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
