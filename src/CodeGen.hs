{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module CodeGen where
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Type.Natural
import           Language.C
import           Language.C.Pretty
import           System.IO.Unsafe

import           CodeGen.Data
import           CodeGen.Monad
import           CodeGen.Type
import           Language.Poly.Core
import           RtDef
import           Rt                             ( checkDual )

data ExtraContext = ExtraContext {
    ruleForPureCg :: CgRule
}

-- specify how instrs are generated when hitting Pure     
data CgRule where
    RReturn :: CgRule -- return statement when reaching Pure
    RAssign :: Int -> CgRule -- Assign value to the variable
    RIgnore :: CgRule -- ignore the result 

instance MonadIO Identity where
    liftIO = Identity . unsafePerformIO

-- error "the list of processes are not dual"
codeGenDebug :: Repr a => [ProcessRT a] -> IO ()
codeGenDebug xs
    | otherwise = codeGen --- | checkDual xs = codeGen
    | otherwise    = putStrLn "List of processes are not dual" >> codeGen
  where
    codeGen = writeFile "codegen/code.c" (headers ++ source ++ "\n")
    source  = (show . pretty . testCodeGen) xs
    headers = concatMap (\x -> "#include<" ++ x ++ ".h>\n")
                        ["stdint", "stdio", "chan", "pthread"]

testCodeGen :: Repr a => [ProcessRT a] -> CTranslUnit
testCodeGen xs = evalCodeGen $ traverseToCodeGen singleType xs

traverseToCodeGen
    :: MonadIO m
    => SingleType a
    -> [ProcessRT a]
    -> CodeGen m [(Nat, (Seq Instr))]
traverseToCodeGen stype = mapM $ uncurry $ helper stype
  where
    defaultContext = ExtraContext { ruleForPureCg = RReturn }

    debug :: (MonadIO m, Show a) => a -> m ()
    debug = (liftIO . putStrLn . show)

    -- helper :: MonadIO m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Nat, Seq Instr)
    helper singType process role =
        fmap (role, ) $ helper_ singType process role defaultContext

    helper_
        :: MonadIO m
        => SingleType a
        -> ProcRT a
        -> Nat
        -> ExtraContext
        -> CodeGen m (Seq Instr)
    helper_ stype (Pure (exp :: Core a)) _ cxt@ExtraContext {..} =
        return $ case ruleForPureCg of
            RReturn         -> Seq.singleton (CEnd stype (Exp exp stype))
            RAssign varName -> Seq.singleton (CAssgn varName $ Exp exp stype)
            RIgnore         -> Seq.empty
    helper_ stype (Free (Send' receiver (exp :: Core a) next)) role cxt = do
        let chanKey = ChanKey { chanCreator = role, chanDestroyer = receiver }
        cid <- getChannelAndUpdateChanTable2 chanKey role
        let chan = Channel cid (singleType :: SingleType a)
        varName <- freshVarName
        let var = Var $ fromIntegral varName
        let instrs = Seq.fromList
                [ CDecla varName (singleType :: SingleType a)
                , CAssgn varName (Exp exp singleType)
                , CSend chan (Exp var singleType)
                ]
        restOfInstrs <- helper_ stype next role cxt
        return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Recv' sender (cont :: Core a -> ProcRT a1))) role cxt
        = do
            let chanKey =
                    ChanKey { chanCreator = sender, chanDestroyer = role }
            cid <- getChannelAndUpdateChanTable2 chanKey role
            let chan = Channel cid (singleType :: SingleType a)
            varName <- freshVarName
            let var = Var (fromIntegral varName)
            let instrs = Seq.fromList
                    [ CDecla varName (singleType :: SingleType a)
                    , CRecv chan (Exp var singleType)
                    ]
            restOfInstrs <- helper_ stype (cont var) role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Branch' sender left right next)) role cxt = do
        let chanKey = ChanKey { chanCreator = sender, chanDestroyer = role }
        cid <- getChannelAndUpdateChanTable2 chanKey role
        let chan = Channel cid singleTypeLabel
        varName <- freshVarName
        let var = Var $ fromIntegral varName :: Core Label
        leftSeqs  <- helper_ singleType left role $ updateIgnore cxt
        rightSeqs <- helper_ singleType right role $ updateIgnore cxt
        let instrs = Seq.fromList
                [ CDecla varName singleTypeLabel
                , CRecv chan (Exp var singleTypeLabel)
                , CBranch (Exp var singleTypeLabel) leftSeqs rightSeqs
                ]
        restOfInstrs <- helper_ stype next role cxt
        return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (BranchCont' sender left (right :: ProcRT c) cont)) role cxt
        = do
            let chanKey =
                    ChanKey { chanCreator = sender, chanDestroyer = role }
            cid <- getChannelAndUpdateChanTable2 chanKey role
            let chan = Channel cid singleTypeLabel
            varName <- freshVarName
            let var = Var $ fromIntegral varName :: Core Label
            varNameForCont <- freshVarName
            let varForCont = Var $ fromIntegral varNameForCont :: Core c
            leftSeqs <- helper_
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
                    , CDecla varName        singleTypeLabel
                    , CRecv chan (Exp var singleTypeLabel)
                    , CBranch (Exp var singleTypeLabel) leftSeqs rightSeqs
                    ]
            restOfInstrs <- helper_ stype (cont varForCont) role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Select' receiver (exp :: Core (Either a b)) left right next)) role cxt
        = do
            varEitherName <- freshVarName
            let chanKey =
                    ChanKey { chanCreator = role, chanDestroyer = receiver }
            cid <- getChannelAndUpdateChanTable2 chanKey role
            let chan = Channel cid singleTypeLabel
            varLabelName <- freshVarName
            let varLabel = Var $ fromIntegral varLabelName :: Core Label
            varLeftVarName  <- freshVarName
            varRightVarName <- freshVarName
            let varLeft  = Var $ fromIntegral varLeftVarName :: Core a
            let varRight = Var $ fromIntegral varRightVarName :: Core b
            leftSeqs <- helper_ singleType (left varLeft) role
                $ updateIgnore cxt
            rightSeqs <- helper_ singleType (right varRight) role
                $ updateIgnore cxt
            updateEitherTypeCollects (singleType :: SingleType (Either a b))
            let
                instrs = Seq.fromList
                    [ CDecla varEitherName
                             (singleType :: SingleType (Either a b))
                    , CAssgn varEitherName $ Exp exp singleType
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
    helper_ stype (Free (SelectMult' receivers (exp :: Core (Either a b)) left right next)) role cxt
        = do
            varEitherName <- freshVarName
            varLabelName  <- freshVarName
            let varLabel = Var $ fromIntegral varLabelName :: Core Label
            sendInstrs <- mapM
                (getSendValueChanInstr (Exp varLabel singleTypeLabel)
                                       singleTypeLabel
                                       role
                )
                receivers
            varLeftVarName  <- freshVarName
            varRightVarName <- freshVarName
            let varLeft  = Var $ fromIntegral varLeftVarName :: Core a
            let varRight = Var $ fromIntegral varRightVarName :: Core b
            leftSeqs <- helper_ singleType (left varLeft) role
                $ updateIgnore cxt
            rightSeqs <- helper_ singleType (right varRight) role
                $ updateIgnore cxt
            updateEitherTypeCollects (singleType :: SingleType (Either a b))
            let
                instrs =
                    Seq.fromList
                        $  [ CDecla varEitherName
                                    (singleType :: SingleType (Either a b))
                           , CAssgn varEitherName $ Exp exp singleType
                           , CDecla varLabelName singleTypeLabel
                           , CEither2Label varEitherName varLabelName
                           ]
                        ++ sendInstrs
                        ++ [ CDecla varLeftVarName  (singleType :: SingleType a)
                           , CDecla varRightVarName (singleType :: SingleType b)
                           , CSelect varEitherName
                                     varLeftVarName
                                     varRightVarName
                                     leftSeqs
                                     rightSeqs
                           ]
            restOfInstrs <- helper_ stype next role cxt
            return (instrs Seq.>< restOfInstrs)
    helper_ stype (Free (Rec' _ next)) role cxt = do
        restOfInstrs <- helper_ stype next role cxt
        return restOfInstrs
    helper_ stype (Free (Mu' _)) role cxt = return $ Seq.singleton $ CRec role

    updateCxt rule a = a { ruleForPureCg = rule }
    updateIgnore = updateCxt RIgnore

    getSendValueChanInstr
        :: Monad m => Exp a -> SingleType a -> Nat -> Nat -> CodeGen m Instr
    getSendValueChanInstr exp sType sender receiver = do
        let chanKey =
                ChanKey { chanCreator = sender, chanDestroyer = receiver }
        cid <- getChannelAndUpdateChanTable2 chanKey sender
        let chan = Channel cid sType
        return $ CSend chan exp
