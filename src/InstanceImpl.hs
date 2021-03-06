{-# LANGUAGE OverloadedStrings #-}

module InstanceImpl (plugin) where

import qualified Data.Generics as G
import Data.List

import GHC.Data.Bag
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Plugins
import GHC.Types.Error
import GHC.Types.SourceText

--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { driverPlugin = \_opts -> pure . addExts
  , parsedResultAction = \_opts _summary -> desugarMod
  , pluginRecompile = purePlugin
  }

--------------------------------------------------------------------------------

data M a = M (Bag (MsgEnvelope DecoratedSDoc)) [a]

instance Functor M where
  fmap f (M errs xs) = M errs (map f xs)

instance Applicative M where
  pure x = M emptyBag [x]
  M ferrs fxs <*> M xerrs xxs = M (unionBags ferrs xerrs) (fxs <*> xxs)

instance Monad M where
  M errs xs >>= f =
    let (errss, xss) = unzip $ map (mtuple . f) xs
    in M (unionManyBags $ errs:errss) (concat xss)
    where mtuple (M errs' xs') = (errs', xs')

instance MonadFail M where
  fail _ = M emptyBag []

mkM :: [a] -> M a
mkM = M emptyBag

errM :: MsgEnvelope DecoratedSDoc -> M a
errM err = M (unitBag err) []

runM :: M a -> Hsc [a]
runM (M errs xs) = Hsc $ \_env msgs -> pure (xs, unionBags errs msgs)

--------------------------------------------------------------------------------

addExts :: HscEnv -> HscEnv
addExts env@(HscEnv { hsc_dflags = dflags }) =
  env { hsc_dflags = xopt_set dflags DataKinds }

desugarMod :: HsParsedModule -> Hsc HsParsedModule
desugarMod hpm@(HsParsedModule { hpm_module = L l hm@(HsModule
    { hsmodImports = imports
    , hsmodDecls = decls
    }) }) = do
  decls' <- runM $ G.everywhereM (G.mkM desugarInst) =<< mkM decls
  pure $ hpm { hpm_module = L l $ hm
   { hsmodImports = extraImports ++ imports
   , hsmodDecls = decls'
   } }

desugarInst :: ClsInstDecl GhcPs -> M (ClsInstDecl GhcPs)
desugarInst ClsInstDecl
  { cid_poly_ty =
    L _ (HsSig _ (HsOuterImplicit _)
      (L _ (HsAppTy _
        (L _ (HsTyVar _ NotPromoted (L _ (Unqual cls))))
        ty)))
  , cid_binds = binds
  , cid_sigs = sigs
  }
  | occNameFS cls == impl
  = do
      L (SrcSpanAnn _ lsig) (ClassOpSig _ _ idps (L _ sigtype)) <- mkM sigs
      L _ (Unqual name) <- mkM idps
      bind <- case bagToList $ mapMaybeBag (morphBind name) binds of
        [] -> errM . mkMsgEnvelope lsig (queryQual defaultErrStyle) $
          "No definition given for impl method" <+> quote (ppr name)
        matchingBinds -> mkM matchingBinds
      let (bndrs, nodep) = sigTyVars sigtype $ tyVars ty
          (ctxs, bareSigtype) = splitQual $ sig_body sigtype
      pure $ ClsInstDecl
        { cid_ext = (EpAnnNotUsed, NoAnnSortKey)
        , cid_poly_ty = gen . HsSig noExtField bndrs
          $ qual (ctxs ++ jailbreakCtx nodep)
          $ hasField `app` symbol name `app` ty `app` bareSigtype
        , cid_binds = unitBag bind
        , cid_sigs = []
        , cid_tyfam_insts = []
        , cid_datafam_insts = []
        , cid_overlap_mode = Nothing
        }
desugarInst inst = pure inst

--------------------------------------------------------------------------------

mkImport :: ModuleName -> LImportDecl GhcPs
mkImport modName = gen $ ImportDecl
  { ideclExt = EpAnnNotUsed
  , ideclSourceSrc = NoSourceText
  , ideclName = gen modName
  , ideclPkgQual = Nothing
  , ideclSource = NotBoot
  , ideclSafe = False
  , ideclQualified = QualifiedPre
  , ideclImplicit = True
  , ideclAs = Nothing
  , ideclHiding = Nothing
  }

extraImports :: [LImportDecl GhcPs]
extraImports = mkImport <$> [ghcRecords, funDepJailbreak]

ghcRecords :: ModuleName
ghcRecords = mkModuleName "GHC.Records"

funDepJailbreak :: ModuleName
funDepJailbreak = mkModuleName "InstanceImpl.FunDepJailbreak"

symbol :: OccName -> LHsType GhcPs
symbol = gen . HsTyLit noExtField . HsStrTy NoSourceText . occNameFS

app :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
app f x = gen $ HsAppTy noExtField f x

qual :: [LHsType GhcPs] -> LHsType GhcPs -> LHsType GhcPs
qual [] x = x
qual cs x = gen $ HsQualTy noExtField (Just $ gen cs) x

hasField :: LHsType GhcPs
hasField
  = gen . HsTyVar EpAnnNotUsed NotPromoted
  . gen . mkRdrQual ghcRecords $ mkClsOcc "HasField"

impl :: FastString
impl = "impl"

sigTyVars
  :: HsSigType GhcPs
  -> [RdrName]
  -> (HsOuterTyVarBndrs Specificity GhcPs, [RdrName])
sigTyVars (HsSig _ (HsOuterExplicit a opBndrs) _) headTvs =
  ( HsOuterExplicit a
    $ map (gen . UserTyVar EpAnnNotUsed InferredSpec . gen) headTvs ++ opBndrs
  , tyVars =<< opBndrs
  )
sigTyVars (HsSig _ hsOuterImplicit ty) headTvs =
  ( hsOuterImplicit
  , nub (tyVars ty) \\ headTvs
  )

tyVars :: G.Data a => a -> [RdrName]
tyVars = G.listify isRdrTyVar

splitQual :: LHsType GhcPs -> ([LHsType GhcPs], LHsType GhcPs)
splitQual (L _ (HsQualTy _ (Just (L _ ctxs)) ty)) = (ctxs, ty)
splitQual ty = ([], ty)

jailbreakCtx :: [RdrName] -> [LHsType GhcPs]
jailbreakCtx = map
  $ app jailbreak . gen
  . HsTyVar EpAnnNotUsed NotPromoted . gen

jailbreak :: LHsType GhcPs
jailbreak
  = gen . HsTyVar EpAnnNotUsed NotPromoted
  . gen . mkRdrQual funDepJailbreak $ mkClsOcc "Jailbreak"

--------------------------------------------------------------------------------

morphBind :: OccName -> LHsBind GhcPs -> Maybe (LHsBind GhcPs)
morphBind method (L l bind@(FunBind
  { fun_id = L _ (Unqual fun)
  , fun_matches = matches@(MG { mg_alts = alts })
  }))
  | method == fun
  = Just . L l $ bind
    { fun_id = gen . Unqual $ mkVarOcc "getField"
    , fun_matches = matches { mg_alts = mapLoc (map $ mapLoc addSelf) alts }
    }
morphBind _ _ = Nothing

addSelf :: Match GhcPs body -> Match GhcPs body
addSelf match@(Match { m_pats = pats }) = match { m_pats = self : pats }

self :: LPat GhcPs
self = gen . VarPat noExtField . gen . Unqual $ mkVarOcc "self"

--------------------------------------------------------------------------------

gen :: a -> GenLocated (SrcSpanAnn' (EpAnn ann)) a
gen = L genSrcSpanAnn

genSrcSpanAnn :: SrcSpanAnn' (EpAnn ann)
genSrcSpanAnn = SrcSpanAnn EpAnnNotUsed genSrcSpan

genSrcSpan :: SrcSpan
genSrcSpan = mkGeneralSrcSpan "<generated by instance-impl>"
