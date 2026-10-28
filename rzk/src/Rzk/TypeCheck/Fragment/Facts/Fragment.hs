{-# LANGUAGE LambdaCase #-}

-- | The certificate of the RSTT fragment check ("Rzk.TypeCheck.Fragment.RSTT"):
-- its walk, over the facts, in both views at once. The views take the same
-- cases with the same children wherever a term exposes to itself, and part
-- only at a term that reduces. The cases follow @checkFragmentUses@ one for
-- one, and every walk of a subterm goes through the subterm's memo.
module Rzk.TypeCheck.Fragment.Facts.Fragment (
  verdictsOf,
  scopedVerdictsOf,
  fragmentVerdict,
) where

import           Rzk.TypeCheck.Context               (FragmentFlags (..))
import           Rzk.TypeCheck.Fragment.Facts.Expose
import           Rzk.TypeCheck.Fragment.Facts.Types

-- | The equation of 'fVerdicts'.
verdictsOf :: Facts -> ByFlags (ByEntry Verdicts)
verdictsOf f = byFlags $ \fl -> byEntry $ \e -> fragmentAt fl e f

-- | The equation of 'fVerdicts' for facts from a table's body at a use: the
-- body's verdicts, settled against the use's arguments.
scopedVerdictsOf :: Frame -> Facts -> ByFlags (ByEntry Verdicts)
scopedVerdictsOf frame g = byFlags $ \fl -> byEntry $ \e ->
  let Verdicts src cmp = fragmentIn fl e g
  in Verdicts (resolveTable fl frame src) (resolveTable fl frame cmp)

-- | The certificate of the fragment check on a declaration: clean in both
-- views, at the entries the standalone walk starts from.
fragmentVerdict :: FragmentFlags -> DeclFacts -> Bool -> Verdict
fragmentVerdict fl decl isAssumption = foldMap inBothEntries [SourceTypes, ComputedTypes]
  where
    entry = if isAssumption then EAssumed OutsideParameterPrefix UseBinder else ETail
    inBothEntries view =
      fragment fl view entry (declType decl)
        <> foldMap (fragment fl view (ETerm InParameterPrefix)) (declValue decl)

-- | The verdict at an entry in a view, from the term's memo.
fragment :: FragmentFlags -> View -> Entry -> Facts -> Verdict
fragment fl view e = inView view . fragmentIn fl e

-- | Both views' verdicts at an entry, from the term's memo.
fragmentIn :: FragmentFlags -> Entry -> Facts -> Verdicts
fragmentIn fl e = atEntry e . atFlags fl . fVerdicts

-- | Settle the needs of a table's body against the arguments of a use, all of
-- them, with the check's flags.
resolveTable :: FragmentFlags -> Frame -> Verdict -> Verdict
resolveTable fl = settleWith (evalReqWith (checkReq fl))

-- | Meet a requirement of the check on an argument.
checkReq :: FragmentFlags -> Facts -> CheckReq -> Verdict
checkReq fl f = \case
  ReqClean view e -> fragment fl view e f
  ReqApplied view e ops -> fragment fl view e (applyOps f ops)
  ReqBoundary ops -> boundaryAt fl ComputedTypes Nothing (applyOps f ops)
  ReqArgumentAt view arg -> inView view (argumentAt fl f arg)

-- | The walk at an entry, in both views.
fragmentAt :: FragmentFlags -> Entry -> Facts -> Verdicts
fragmentAt fl = \case
  ETail -> tailAt
  EAssumed p u -> assumedAt p u
  EExtCodomain u -> extCodomainAt u
  ETerm p -> termAt p
  EData p -> dataAt p
  where
    goTail = fragmentIn fl ETail
    goAssumed p u = fragmentIn fl (EAssumed p u)
    goExtCodomain u = fragmentIn fl (EExtCodomain u)
    goTerm p = fragmentIn fl (ETerm p)
    goData p = fragmentIn fl (EData p)

    out = OutsideParameterPrefix

    -- The faces of a restriction are terms.
    faceTerms = foldMap (\(Face tope term) -> goTerm out tope <> goTerm out term)

    -- The source view takes the term as it is; the computed view exposes it:
    -- a definition's application is answered by its table, a parameter by
    -- its argument.
    exposedAt :: Entry -> Facts -> (Facts -> Verdicts) -> Verdicts
    exposedAt e f k = case fExpose f of
      Exposed steps ns (ToFacts origin g)
        | steps >= exposeLimit -> computed nearLimit
        | origin == Itself ->
            let r = k f in Verdicts (sourceVerdict r) (needs ns <> computedVerdict r)
        | otherwise -> computed (needs ns <> computedVerdict (k g))
      Exposed steps ns (ToTable frame)
        | steps < exposeLimit -> computed (needs ns <> inBody frame (tableBody frame))
        | otherwise -> computed nearLimit
      Exposed steps ns (InTable frame g)
        | steps < exposeLimit -> computed (needs ns <> inBody frame g)
        | otherwise -> computed nearLimit
      Exposed _ ns (ToParam p) ->
        computed (needs (ns <> [Need p (ReqCheck (ReqClean ComputedTypes e))]))
      Exposed _ ns (ToParamApp p ops) ->
        computed (needs (ns <> [Need p (ReqCheck (ReqApplied ComputedTypes e ops))]))
      Exposed _ _ (ToParamType _) -> computed (Unclean "the type of an argument, exposed")
      ExposedUnknown r -> computed (Unclean r)
      where
        computed = Verdicts (sourceVerdict (k f))
        nearLimit = Unclean "near the reduction limit"
        inBody frame g = resolveTable fl frame (fragment fl ComputedTypes e g)

    -- Allow restrictions along the spine of concluded codomains.
    tailAt :: Facts -> Verdicts
    tailAt f = exposedAt ETail f $ \t -> case fShape t of
      SRestricted ty faces -> goTail ty <> faceTerms faces
      SPi p ->
        shapeDependencies False t <> goAssumed out UseBinder (piDomain p)
          <> foldMap (goTerm out) (piTope p) <> goTail (piCodomain p)
      SRecOr faces -> foldMap (\(Face tope term) -> goTerm out tope <> goTail term) faces
      _ -> goAssumed out UseConcluded t

    -- Assumed restrictions must sit directly under a shape-Π.
    assumedAt :: PrefixPosition -> FragmentUse -> Facts -> Verdicts
    assumedAt pos u f = exposedAt (EAssumed pos u) f $ \t -> case fShape t of
      SRestricted ty faces -> both freeStanding <> goAssumed out u ty <> faceTerms faces
      SPi p ->
        let (shape, vs) = isShapeBinder (piTope p) (piDomain p)
        in shapeDependencies True t <> goAssumed out UseBinder (piDomain p)
             <> foldMap (goTerm out) (piTope p) <> both vs
             <> (if shape then goExtCodomain u else goAssumed out u) (piCodomain p)
      SSigma s -> goAssumed out UseBinder (sigmaFirst s) <> goAssumed out u (sigmaSecond s)
      -- Identity-type arguments must be ext-style; endpoints remain terms.
      SId i ->
        goTerm out (idLeft i) <> foldMap (goAssumed out UseIdentity) (idType i)
          <> goTerm out (idRight i)
      SLam l -> lambdaBinder pos (lamParam l) <> goAssumed pos u (lamBody l)
      STypeModal ty -> goAssumed out u ty
      SRecOr faces -> foldMap (\(Face tope term) -> goTerm out tope <> goAssumed out u term) faces
      SVar _ -> mempty
      _ -> goTerm out t

    -- Direct shape codomains admit boundary clipping to the binder's domain.
    extCodomainAt :: FragmentUse -> Facts -> Verdicts
    extCodomainAt u f = exposedAt (EExtCodomain u) f $ \t -> case fShape t of
      SRestricted ty faces -> goAssumed out u ty <> faceTerms faces
      _ -> goAssumed out u t

    -- The binder of a λ: its domain is assumed, its tope is a term, and
    -- outside a parameter prefix it must not be schematic.
    lambdaBinder :: PrefixPosition -> Maybe LamParam -> Verdicts
    lambdaBinder pos = \case
      Nothing -> mempty
      Just (LamParam dom mtope) ->
        goAssumed out UseBinder dom <> foldMap (goTerm out) mtope
          <> both (if pos == out then metaBinder dom else Clean)

    -- RS17 Appendix A.2 requires shapes independent of ambient cube points.
    -- Schematic families are judgements over cube contexts, not extension types.
    shapeDependencies :: Bool -> Facts -> Verdicts
    shapeDependencies assumed t
      | not (flagShapeDependency fl) = mempty
      | SPi p <- fShape t =
          let (schematic, vs1) = decide (fMeta t)
              (shape, vs2) = isShapeBinder (piTope p) (piDomain p)
              bound = Just (piVar p)
              noPoints = noPointsBut bound . fPoints
          in both (vs1 <> vs2) <> if not shape then mempty else
               both (foldMap (\tope -> if schematic then Clean else noPoints tope) (piTope p)
                       <> noPoints (piDomain p))
                 <> (if schematic || not assumed then mempty else
                       Verdicts (boundaryAt fl SourceTypes bound (piCodomain p))
                                (boundaryAt fl ComputedTypes bound (piCodomain p)))
      | otherwise = mempty

    -- Check term binders and route type-valued bodies through goData.
    termAt :: PrefixPosition -> Facts -> Verdicts
    termAt InParameterPrefix f | not (isLambda f) = goData InParameterPrefix f
    termAt pos f = case fShape f of
      SVar _ -> mempty
      SParam p -> inBothViews $ \view -> needs [Need p (ReqCheck (ReqClean view (ETerm pos)))]
      SParamType _ _ -> both (Unclean "the type of an argument, as a term")

      -- Function types returned by lets or branches still introduce shapes.
      SPi{} -> goAssumed out UseData f

      -- The stored type is synthesised by elaboration; it is not assumed.
      SRefl me -> foldMap (goTerm out . endpointTerm) me

      STypeAsc term ty -> goTerm out term <> goTail ty

      SLam l ->
        (case fShape (fType f) of
           SPi{} -> shapeDependencies False (fType f)
           _     -> mempty)
          <> lambdaBinder pos (lamParam l) <> goTerm pos (lamBody l)

      SApp{} -> case fShape h of
        -- An argument applied inside a definition: the walk sees the
        -- application of the actual argument.
        SParam p -> inBothViews $ \view ->
          needs [Need p (ReqCheck (ReqApplied view (ETerm pos) (map (OpApp . snd) args)))]
        _ -> goTerm out h <> foldMap argument args
        where
          (h, args) = spine f

      -- The motive is assumed: the eliminator binds its variables at it.
      SIdJ j ->
        goData out (jType j) <> goTerm out (jStart j) <> goAssumed out UseMotive (jMotive j)
          <> goTerm out (jBase j) <> goTerm out (jEnd j) <> goTerm out (jProof j)

      SPair l r -> goData out l <> goData out r

      -- The let annotation records the value's concluded type.
      SLet l ->
        foldMap goTail (letAnnotation l) <> goData out (letValue l) <> goTerm out (letBody l)

      -- Every other node: its unscoped children are terms.
      SUniverse -> mempty
      SCubeU -> mempty
      STopeU -> mempty
      SLeaf _ -> mempty
      SCubeOrTope _ cs -> foldMap (goTerm out) cs
      SSigma s -> goTerm out (sigmaFirst s)
      SId i -> goTerm out (idLeft i) <> foldMap (goTerm out) (idType i) <> goTerm out (idRight i)
      SRestricted ty faces -> goTerm out ty <> faceTerms faces
      SRecOr faces -> faceTerms faces
      SFirst t -> goTerm out t
      SSecond t -> goTerm out t
      STypeModal ty -> goTerm out ty
      SScoped frame g ->
        let Verdicts src cmp = goTerm pos g
        in Verdicts (resolveTable fl frame src) (resolveTable fl frame cmp)
      SOther r -> both (Unclean r)

    -- An argument, given the function node it is applied to.
    argument :: (Facts, Facts) -> Verdicts
    argument (fnode, arg) = argumentAt fl (fType fnode) arg

    -- Type-valued data may later become binder or motive types. A proof
    -- quantified over a universe must still be walked as a term.
    dataAt :: PrefixPosition -> Facts -> Verdicts
    dataAt pos f = case fShape f of
      SParam p -> inBothViews $ \view -> needs [Need p (ReqCheck (ReqClean view (EData pos)))]
      _ ->
        let (isType, vs) = decide (fLands (fType f))
        in both vs <> (if isType then goAssumed pos UseData f else goTerm out f)

    isLambda f = case fShape f of
      SLam{} -> True
      _      -> False

    -- A cube domain without a tope denotes the unrestricted shape.
    isShapeBinder :: Maybe Facts -> Facts -> (Bool, Verdict)
    isShapeBinder (Just _) _ = (True, Clean)
    isShapeBinder Nothing dom = decide (fCube (fType dom))

    metaBinder :: Facts -> Verdict
    metaBinder dom
      | not (flagMetaBinder fl) = Clean
      | otherwise =
          let (meta, vs) = decide (fMeta dom)
          in vs <> (if meta then Unclean "a schematic binder inside a term" else Clean)

    freeStanding :: Verdict
    freeStanding
      | flagFreeStandingRestriction fl = Unclean "a free-standing restriction"
      | otherwise = Clean

-- | A verdict per view.
inBothViews :: (View -> Verdict) -> Verdicts
inBothViews k = Verdicts (k SourceTypes) (k ComputedTypes)

-- | The application rule's checks of an argument, given the function's type
-- (the standalone walk's @goArgument@), in both views.
argumentAt :: FragmentFlags -> Facts -> Facts -> Verdicts
argumentAt fl ty arg = case domainOf ty of
  DomainOf _ _ (NoDomain r) -> both (Unclean r)
  -- The type is a parameter's argument: the use settles it.
  DomainOf frames ns (DomainOfParam p ns') -> inBothViews $ \view ->
    settledIn frames (needs (ns <> ns' <> [Need p (ReqCheck (ReqArgumentAt view arg))]))
  -- Substitution can create a forbidden type in a later argument domain,
  -- which the computed view checks.
  DomainOf frames ns (Domain mdom) ->
    Verdicts
      (settledIn frames (needs ns))
      (settledIn frames (needs ns <> foldMap (fragment fl ComputedTypes assumedBinder) mdom))
      -- Schema arguments may bind their own schematic parameters.
      <> let (plumbing, vs) = decide (scopedSym frames (maybe (known False) fMeta mdom))
             route
               | plumbing = fragmentIn fl (ETerm InParameterPrefix) arg
               | otherwise = fragmentIn fl (EData OutsideParameterPrefix) arg
         in both vs <> route
  where
    settledIn frames v = foldl' (flip (resolveTable fl)) v frames
    assumedBinder = EAssumed OutsideParameterPrefix UseBinder

-- | The boundary of an assumed extension type must not mention outer points:
-- the codomain, exposed, is either not a restricted type or one whose faces
-- reach no cube point but the binder's own.
boundaryAt :: FragmentFlags -> View -> Maybe VarFacts -> Facts -> Verdict
boundaryAt fl view bound cod = case view of
  SourceTypes -> faces cod
  ComputedTypes -> case fExpose cod of
    Exposed steps ns (ToFacts _ body)
      | steps < exposeLimit -> needs ns <> faces body
      | otherwise -> nearLimit
    Exposed steps ns (ToTable frame)
      | steps < exposeLimit -> needs ns <> inBody frame (tableBody frame)
      | otherwise -> nearLimit
    Exposed steps ns (InTable frame g)
      | steps < exposeLimit -> needs ns <> inBody frame g
      | otherwise -> nearLimit
    Exposed _ ns (ToParam p) -> needs (ns <> [Need p (ReqCheck (ReqBoundary []))])
    Exposed _ ns (ToParamApp p ops) -> needs (ns <> [Need p (ReqCheck (ReqBoundary ops))])
    Exposed _ _ (ToParamType _) -> Unclean "a boundary that is an argument's type"
    ExposedUnknown r -> Unclean r
  where
    nearLimit = Unclean "near the reduction limit"
    inBody frame g = resolveTable fl frame (boundaryAt fl view Nothing g)
    faces body = case fShape body of
      SRestricted _ fs -> foldMap (noPointsBut bound . fPoints . faceTope) fs
      _                -> Clean
