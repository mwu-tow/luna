module Luna.Runtime.Backend.GHCCore where

import Prologue hiding (putStrLn, def)

-- Compiler
import GHC hiding (def)
import DynFlags
import HscTypes
import Outputable
import GHC.Paths ( libdir )

-- Core Types
import Var
import Name
import Avail
import IdInfo
import Type
import Module
import Unique
import OccName
import InstEnv
import RdrName
import FamInstEnv
import qualified Stream
import qualified CoreSyn as Syn

-- Core Passes
import CorePrep (corePrepPgm)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import StgCmm (codeGen)
import CmmInfo (cmmToRawCmm )
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT, srtToData)
import AsmCodeGen ( nativeCodeGen )
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import System.IO
import Data.Time



-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

mkName :: Int -> String -> Name
mkName i n =
    mkInternalName (mkUnique 'n' i) (mkOccName OccName.varName n) noSrcSpan

xn :: Name
xn = mkName 0 "x"

an :: Name
an = mkName 1 "a"

fn :: Name
fn = mkExternalName (mkUnique 'n' 2) modl (mkOccName OccName.varName "f")
    noSrcSpan

-- a :: *
a :: TyVar
a = mkTyVar an placeHolderTypeTc

-- x :: a
x :: Var
x = mkLocalVar VanillaId xn (varType a) vanillaIdInfo

-- f :: a -> a
fv :: Var
fv = mkGlobalVar VanillaId fn (mkFunTy (varType a) (varType a)) vanillaIdInfo

def :: [Syn.CoreBind]
def = [Syn.NonRec fv f]

f :: Syn.Expr Var
f = Syn.Lam x (Syn.Var x)

modl :: Module
modl = mkModule mainUnitId (mkModuleName "Example")

guts :: ModGuts
guts = ModGuts
    { mg_module          = modl
    , mg_hsc_src         = HsSrcFile
    , mg_loc             = noSrcSpan
    , mg_exports         = [Avail fn]
    , mg_deps            = noDependencies
    , mg_usages          = [] -- TODO [Ara] Not correct
    , mg_used_th         = False
    , mg_rdr_env         = emptyGlobalRdrEnv
    , mg_fix_env         = emptyFixityEnv
    , mg_tcs             = []
    , mg_insts           = []
    , mg_fam_insts       = []
    , mg_patsyns         = []
    , mg_rules           = []
    , mg_binds           = def
    , mg_foreign         = NoStubs
    , mg_foreign_files   = []
    , mg_warns           = NoWarnings
    , mg_anns            = []
    , mg_complete_sigs   = []
    , mg_hpc_info        = NoHpcInfo False
    , mg_modBreaks       = Nothing
    , mg_vect_decls      = []
    , mg_vect_info       = noVectInfo
    , mg_inst_env        = emptyInstEnv
    , mg_fam_inst_env    = emptyFamInstEnv
    , mg_safe_haskell    = Sf_None
    , mg_trust_pkg       = False }

summ :: DynFlags -> ModSummary
summ dflags = ModSummary
    { ms_mod          = modl
    , ms_hsc_src      = HsSrcFile
    , ms_location     = ModLocation
        { ml_hs_file  = Nothing
        , ml_hi_file  = "Example.hi"
        , ml_obj_file = "Example.o" }
    , ms_hs_date      = UTCTime (toEnum 0) 0
    , ms_obj_date     = Nothing
    , ms_iface_date   = Nothing
    , ms_srcimps      = []
    , ms_textual_imps = []
    , ms_parsed_mod   = Nothing
    , ms_hspp_file    = "Example.hs"
    , ms_hspp_opts    = dflags
    , ms_hspp_buf     = Nothing }

modloc :: ModLocation
modloc = ModLocation
   { ml_hs_file  = Nothing
   , ml_hi_file  = "Example.hi"
   , ml_obj_file = "Example.o" }

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

test :: IO ()
test = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags

    setSessionDynFlags $ dflags { hscTarget = HscAsm, ghcLink = LinkBinary }

    dflags <- getSessionDynFlags
    env <- getSession

    setTargets [Target
        { targetId = TargetModule (mkModuleName "Example")
        , targetAllowObjCode = True
        , targetContents = Nothing }]

    -- Run the Core prep pass
    (prep, _) <- liftIO $ corePrepPgm env modl (ms_location (summ dflags))
        (mg_binds guts) (mg_tcs guts)

    -- Transform Core into STG
    let (stg, cost_centre_info) = coreToStg dflags modl prep

    -- STG Transformer
    stg_binds2 <- liftIO $ stg2stg dflags stg

    -- Generated Cmm
    let cmms = codeGen dflags modl (mg_tcs guts) cost_centre_info stg_binds2
            (mg_hpc_info guts)

    -- Initialize a name supply for the Cmm pipeline
    us <- liftIO $ mkSplitUniqSupply 'S'
    let initTopSRT = initUs_ us emptySRT
        run_pipeline = cmmPipeline env

    -- Collect the Cmm (code stream) after (running the pipeline.)
    let cmmstream = do
         a <- Stream.mapAccumL run_pipeline initTopSRT cmms
         Stream.yield (srtToData a)

    -- Prepare the Cmm for
    genraw <- liftIO $ cmmToRawCmm dflags cmmstream

    -- Initialize name supply for the native code generator and generate x86 to a
    -- file from the prepared Cmm.
    ncg_uniqs <- liftIO $ mkSplitUniqSupply 'n'
    fname <- liftIO $ (openFile "Example.asm" WriteMode)
    liftIO $ nativeCodeGen dflags (mg_module guts) modloc fname ncg_uniqs genraw

    -- Dump the outputted Stg and  Cmm out
    gen <- liftIO $ Stream.collect cmmstream
    liftIO $ putStrLn "=== STG ==="
    liftIO $ putStrLn $ showGhc stg_binds2

    liftIO $ putStrLn "=== CMM ==="
    liftIO $ putStrLn $ showGhc gen

    pure ()

