{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module CodeGen where
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Type.Natural
import           Language.C
import           System.Process
import           System.Exit
import           System.FilePath
import           System.Directory
import           Data.List.Split
import           System.IO.Unsafe

import           CodeGen.Data
import           CodeGen.Monad
import           CodeGen.Type
import           Language.Poly.Core2
import           RtDef
import           Rt                             ( checkDual )
import           ParPattern

data ExtraContext = ExtraContext {
    ruleForPureCg :: CgRule
}

-- specify how instrs are generated when hitting Pure     
data CgRule where
    RReturn :: CgRule -- return statement when reaching Pure
    RAssign :: Int -> CgRule -- Assign value to the variable
    RIgnore :: CgRule -- ignore the result 

codeGenTest :: Serialise a => a -> ArrowPipe a b -> FilePath -> [Double]
codeGenTest a arrow path =
    unsafePerformIO $ codeGenTestCompile a arrow path >> codeGenTestRun path

codeGenTest2 :: Serialise a => a -> ArrowPipe a b -> FilePath -> IO [Double]
codeGenTest2 a arrow path = codeGenTestCompile a arrow path >> codeGenTestRun path

codeGenTestCompile :: Serialise a => a -> ArrowPipe a b -> FilePath -> IO ()
codeGenTestCompile a arrow path =
    codeGenBenchCompile1 a (runPipe1 zero arrow) path

codeGenDebug :: Bool -> [AProcessRT] -> IO ()
codeGenDebug isDebug xs = codeGenHelper defaultHeaders evalCodeGen xs isDebug

codeGenDebug1 :: Bool -> ([AProcessRT], EntryRole a b) -> IO ()
codeGenDebug1 isDebug (xs, entry) =
    codeGenHelper defaultHeaders (evalCodeGen1 entry) xs isDebug

codeGenBenchCompile
    :: (Serialise a) => a -> ([AProcessRT], EntryRole a b) -> FilePath -> IO ()
codeGenBenchCompile sourceData (xs, entry) dir =
    createDirectoryIfMissing True dir
        >> writeSource
        >> codeGenBenchBuildFile dir
  where
    mainAST        = benchMain sourceData
    ma             = traverseToCodeGen xs
    (sourceAST, _) = evalCodeGen2 mainAST entry ma
    sourcePath     = dir </> "code.c"
    writeSource =
        writeFile sourcePath (headers ++ (show $ pretty sourceAST) ++ "\n")
    headers = concatMap
        ((++ "\n") . ("#include" ++))
        (  defaultHeaders
        ++ fmap ((++ "\"") . ("\"" ++)) ["../data.h", "../func.h"]
        )

codeGenBenchCompile1
    :: (Serialise a) => a -> ([AProcessRT], EntryRole a b) -> FilePath -> IO ()
codeGenBenchCompile1 sourceData (xs, entry) dir =
    createDirectoryIfMissing True dir
        >> writeSource
        >> writeHeader
        >> codeGenBenchBuildFile dir
  where
    mainAST                = benchMain sourceData
    ma                     = traverseToCodeGen xs
    (sourceAST, headerAST) = evalCodeGen2 mainAST entry ma
    sourcePath             = dir </> "code.c"
    writeSource =
        writeFile sourcePath (headers ++ (show $ pretty sourceAST) ++ "\n")
    writeHeader = do
        let dataDir = (takeDirectory dir </> "data.h")
        isExist <- doesFileExist dataDir
        if isExist
            then writeFile dataDir dataHeader-- return ()
            else writeFile dataDir dataHeader
    headers = concatMap
        ((++ "\n") . ("#include" ++))
        (  defaultHeaders
        ++ fmap ((++ "\"") . ("\"" ++)) ["../data.h", "../func.h"]
        )
    dataHeader = addIncludeGuard $ show $ pretty headerAST
    addIncludeGuard x = "#ifndef DATA_H\n#define DATA_H\n" ++ x ++ "\n#endif\n"

-- error "the list of processes are not dual"
-- | checkDual xs = codeGen
codeGenHelper
    :: [String]
    -> (CodeGen IO [(Nat, Seq Instr)] -> CTranslUnit)
    -> [AProcessRT]
    -> Bool
    -> IO ()
codeGenHelper headers eval xs isDebug
    | True || isDual = codeGen
    | otherwise      = putStrLn "processes not dual" >> codeGen
  where
    isDual = checkDual $ fmap f xs
    f (AProcRT _ process, role) = (ignoreOutput process, role)
    codeGen = (if isDebug then makeTrue else return ())
        >> writeFile "codegen/code.c" (headerPretty ++ source ++ "\n")
    source       = (show . pretty) $ eval $ traverseToCodeGen xs
    headerPretty = concatMap ((++ "\n") . ("#include" ++)) headers

defaultHeaders :: [String]
defaultHeaders = fmap (\x -> "<" ++ x ++ ".h>")
                      ["stdint", "stdio", "stdlib", "chan", "pthread"]

codeGenBuildRunBench
    :: Serialise a
    => a
    -> ([AProcessRT], EntryRole a b)
    -> FilePath
    -> IO Double
codeGenBuildRunBench sourceData xs path = do
    codeGenBenchCompile sourceData xs path
    codeGenBenchRun path

codeGenBenchBuildFile :: FilePath -> IO ()
codeGenBenchBuildFile path = do
    (rc, _, _) <- readCreateProcessWithExitCode
        (shell $ "make build SRC=" ++ path)
        []
    case rc of
        ExitSuccess -> return ()
        _           -> error "build failed"

-- codeGenBenchRun :: FilePath -> IO Double
-- codeGenBenchRun path = do
--     (rc, _, _) <- readCreateProcessWithExitCode
--             (shell $ "make build SRC=" ++ path)
--             []
--     case rc of
--         ExitSuccess -> do
--             (rcC, output, _) <- readCreateProcessWithExitCode
--                 (shell $ "make run SRC=" ++ path)
--                 []
--             case rcC of
--                 ExitSuccess -> (return $ read $ helper output)
--                 _           -> error "runtime error"
--         _ -> error "build failed"
--       where
--         helper input = last $ init $ splitOn "\n" input
--         rmDir = removeDirectoryRecursive path

codeGenBenchRun :: FilePath -> IO Double
codeGenBenchRun path = do
    (rcC, output, _) <- readCreateProcessWithExitCode
        (shell $ "make run SRC=" ++ path)
        []
    case rcC of
        ExitSuccess -> (return $ read $ helper output)
        _           -> error "runtime error"
  where
    helper input = last $ init $ splitOn "\n" input
    rmDir = removeDirectoryRecursive path

codeGenTestRun :: FilePath -> IO [Double]
codeGenTestRun path = do
    (rcC, output, _) <- readCreateProcessWithExitCode
        (shell $ "make run SRC=" ++ path)
        []
    case rcC of
        ExitSuccess -> (return $ fmap read $ helper output)
        _           -> error "runtime error"
  where
    helper input = tail $ init $ splitOn "\n" input
    rmDir = removeDirectoryRecursive path

codeGenBuildRun :: Serialise a => [ProcessRT a] -> IO Bool
codeGenBuildRun = codeGenBuildRun' . fmap (\(x, y) -> (toAProc x, y))

codeGenBuildRun' :: [AProcessRT] -> IO Bool
codeGenBuildRun' xs = do
    codeGenDebug False xs
    (rc, _, _) <- readCreateProcessWithExitCode (shell "make build") []
    case rc of
        ExitSuccess -> do
            (rcC, _, _) <- readCreateProcessWithExitCode (shell "make run") []
            case rcC of
                ExitSuccess -> return True
                _           -> putStrLn "Failed at the runtime" >> return False
        _ -> putStrLn "Failed at the build time" >> return False

traverseToCodeGen :: MonadIO m => [AProcessRT] -> CodeGen m [(Nat, (Seq Instr))]
traverseToCodeGen ps = mapM (uncurry $ helper) ps
  where
    defaultContext = ExtraContext { ruleForPureCg = RReturn }

    debug :: (MonadIO m, Show a) => a -> m ()
    debug = (liftIO . putStrLn . show)

    -- helper :: MonadIO m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Nat, Seq Instr)
    helper (AProcRT _ process) role =
        fmap (role, ) $ helper_ singleType process role defaultContext

    -- TODO very inefficient way to update data struct collects update on every send, select and pure
    helper_
        :: (MonadIO m, Serialise a)
        => SingleType a
        -> ProcRT a
        -> Nat
        -> ExtraContext
        -> CodeGen m (Seq Instr)
    helper_ stype (Pure (expr :: Core a)) _ ExtraContext {..} = do
        updateDataStructCollectFromCore expr
        updateDataStructCollect stype
        return $ case ruleForPureCg of
            RReturn       -> Seq.empty -- Seq.singleton (CEnd (Exp expr stype))
            RAssign varId -> Seq.singleton (CAssgn varId $ Exp expr stype)
            RIgnore       -> Seq.empty
    helper_ stype (Free (Send' receiver (expr :: Core a) next)) role cxt = do
        updateDataStructCollectFromCore expr
        updateDataStructCollect (singleType :: SingleType a)
        chan         <- getChannel role receiver
        (varId, var) <- getNewVar
        let instrs = Seq.fromList
                [ CDecla varId (singleType :: SingleType a)
                , CAssgn varId (Exp expr singleType)
                , CSend chan (Exp var (singleType :: SingleType a))
                ]
        restOfInstrs <- helper_ stype next role cxt
        return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Recv' sender (cont :: Core a -> ProcRT a1))) role cxt
        = do
            chan         <- getChannel sender role
            (varId, var) <- getNewVar
            let instrs = Seq.fromList
                    [ CDecla varId (singleType :: SingleType a)
                    , CRecv chan (Exp var singleType)
                    ]
            restOfInstrs <- helper_ stype (cont var) role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Branch' sender left right next)) role cxt = do
        chan         <- getChannel sender role
        (varId, var) <- getNewVar
        leftSeqs     <- helper_ singleType left role $ updateIgnore cxt
        rightSeqs    <- helper_ singleType right role $ updateIgnore cxt
        let instrs = Seq.fromList
                [ CDecla varId singleTypeLabel
                , CRecv chan (Exp var singleTypeLabel)
                , CBranch (Exp var singleTypeLabel) leftSeqs rightSeqs
                ]
        restOfInstrs <- helper_ stype next role cxt
        return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (BranchCont' sender left (right :: ProcRT c) cont)) role cxt
        = do
            chan                         <- getChannel sender role
            (varId         , var       ) <- getNewVar
            (varNameForCont, varForCont) <- getNewVar
            leftSeqs                     <- helper_
                singleType
                left
                role
                (cxt { ruleForPureCg = RAssign varNameForCont })
            rightSeqs <- helper_
                singleType
                right
                role
                (cxt { ruleForPureCg = RAssign varNameForCont })
            let instrs = Seq.fromList
                    [ CDecla varNameForCont (singleType :: SingleType c)
                    , CDecla varId          singleTypeLabel
                    , CRecv chan (Exp var singleTypeLabel)
                    , CBranch (Exp var singleTypeLabel) leftSeqs rightSeqs
                    ]
            restOfInstrs <- helper_ stype (cont varForCont) role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Select' receiver (expr :: Core (Either a b)) left right next)) role cxt
        = do
            updateDataStructCollectFromCore expr
            updateDataStructCollect (singleType :: SingleType (Either a b))
            chan                        <- getChannel role receiver
            (varEitherName  , _       ) <- getNewVar
            (varLabelName   , varLabel) <- getNewVar
            (varLeftVarName , varLeft ) <- getNewVar
            (varRightVarName, varRight) <- getNewVar
            leftSeqs <- helper_ singleType (left varLeft) role
                $ updateIgnore cxt
            rightSeqs <- helper_ singleType (right varRight) role
                $ updateIgnore cxt
            let
                instrs = Seq.fromList
                    [ CDecla varEitherName
                             (singleType :: SingleType (Either a b))
                    , CAssgn varEitherName $ Exp expr singleType
                    , CDecla varLabelName singleTypeLabel
                    , CEither2Label varEitherName varLabelName
                    , CSend chan (Exp varLabel singleTypeLabel)
                    , CDecla varLeftVarName  (singleType :: SingleType a)
                    , CDecla varRightVarName (singleType :: SingleType b)
                    , CSelect varEitherName
                              varLeftVarName
                              varRightVarName
                              leftSeqs
                              rightSeqs
                    ]
            restOfInstrs <- helper_ stype next role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (SelectMult' receivers (expr :: Core (Either a b)) left (right :: Core
            b
        -> ProcRT c) next)) role cxt
        = do
            updateDataStructCollectFromCore expr
            updateDataStructCollect (singleType :: SingleType (Either a b))
            (varEitherName  , _         ) <- getNewVar
            (varLabelName   , varLabel  ) <- getNewVar
            (varLeftVarName , varLeft   ) <- getNewVar
            (varRightVarName, varRight  ) <- getNewVar
            (varNameForCont , varForCont) <- getNewVar
            sendInstrs                    <- mapM
                (getSendValueChanInstr (Exp varLabel singleTypeLabel)
                                       singleTypeLabel
                                       role
                )
                receivers
            leftSeqs <-
                helper_ singleType (left varLeft) role
                    $ (cxt { ruleForPureCg = RAssign varNameForCont })
            rightSeqs <-
                helper_ singleType (right varRight) role
                    $ (cxt { ruleForPureCg = RAssign varNameForCont })
            let
                instrs =
                    Seq.fromList
                        $  [ CDecla varEitherName
                                    (singleType :: SingleType (Either a b))
                           , CAssgn varEitherName $ Exp expr singleType
                           , CDecla varLabelName singleTypeLabel
                           , CEither2Label varEitherName varLabelName
                           ]
                        ++ sendInstrs
                        ++ [ CDecla varLeftVarName  (singleType :: SingleType a)
                           , CDecla varRightVarName (singleType :: SingleType b)
                           , CDecla varNameForCont  (singleType :: SingleType c)
                           , CSelect varEitherName
                                     varLeftVarName
                                     varRightVarName
                                     leftSeqs
                                     rightSeqs
                           ]
            restOfInstrs <- helper_ stype (next varForCont) role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Rec' _ next)) role cxt = do
        restOfInstrs <- helper_ stype next role cxt
        return restOfInstrs
    helper_ _stype (Free (Mu' _)) role _cxt =
        return $ Seq.singleton $ CRec role
    helper_ stype (Free (ForcedEval' (val :: Core a) cont)) role cxt = do
        (varId, var) <- getNewVar
        let instrs = Seq.fromList
                [ CDecla varId (singleType :: SingleType a)
                , CAssgn varId $ Exp val singleType
                ]
        restOfInstrs <- helper_ stype (cont var) role cxt
        return $ instrs Seq.>< restOfInstrs

    updateCxt rule a = a { ruleForPureCg = rule }
    updateIgnore = updateCxt RIgnore
