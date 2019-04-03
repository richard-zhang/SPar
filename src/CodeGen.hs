{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module CodeGen where
import Control.Monad.Free
import Control.Monad.Identity
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Type.Natural
import Language.C
import Language.C.Pretty

import CodeGen.Data
import CodeGen.Monad
import CodeGen.Type
import Language.Poly.Core
import RtDef

codeGenDebug :: Repr a => [ProcessRT a] -> IO ()  
codeGenDebug xs = do
    let source = (show . pretty . testCodeGen) xs
    let headers = concatMap (\x -> "#include<" ++ x ++ ".h>\n") ["stdint", "stdio", "chan", "pthread"]
    writeFile "codegen/code.c" (headers ++ source ++ "\n")

testCodeGen :: Repr a => [ProcessRT a] -> CTranslUnit
testCodeGen xs = evalCodeGen $ traverseToCodeGen singleType xs

traverseToCodeGen :: Monad m => SingleType a -> [ProcessRT a] -> CodeGen m [(Nat, (Seq Instr))]
traverseToCodeGen stype = flip forM $ uncurry $ helper stype

helper :: Monad m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Nat, Seq Instr)
helper singType process role = fmap (role,) $ helper_ singType process role

helper_ :: Monad m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Seq Instr)
helper_ stype (Pure exp) _ =  return $ Seq.singleton (CEnd stype (Exp exp stype))
helper_ stype (Free (Send' receiver (exp :: Core a) next)) role = do
    let chanKey = ChanKey { chanCreator = role , chanDestroyer = receiver}
    cid <- getChannelAndUpdateChanTable chanKey role
    let chan = Channel cid (singleType :: SingleType a)
    varName <- freshVarName
    let var = Var $ fromIntegral varName
    let instrs = Seq.fromList [CDecla varName (singleType :: SingleType a),
                               CAssgn varName (Exp exp singleType),
                               CSend chan (Exp var singleType)
                              ]
    restOfInstrs <- helper_ stype next role
    return (instrs Seq.>< restOfInstrs)
helper_ stype (Free (Recv' sender (cont :: Core a -> ProcRT a1) )) role = do
    let chanKey = ChanKey { chanCreator = sender, chanDestroyer = role}
    cid <- getChannelAndUpdateChanTable chanKey role
    let chan = Channel cid (singleType :: SingleType a)
    varName <- freshVarName
    let var = Var (fromIntegral varName)
    let instrs = Seq.fromList [CDecla varName (singleType :: SingleType a), 
                               CRecv chan (Exp var singleType),
                               CDeleteChan chan
                              ]
    restOfInstrs <- helper_ stype (cont var) role
    return (instrs Seq.>< restOfInstrs)
helper_ stype (Free (Branch' sender left right next)) role = do
    let chanKey = ChanKey { chanCreator = sender, chanDestroyer = role}
    cid <- getChannelAndUpdateChanTable chanKey role
    let chan = Channel cid singleTypeLabel
    varName <- freshVarName
    let var = Var $ fromIntegral varName :: Core Label
    leftSeqs <- removeLastSeqSafe <$> helper_ singleType left role
    rightSeqs <- removeLastSeqSafe <$> helper_ singleType right role
    let instrs = Seq.fromList [CDecla varName singleTypeLabel, 
                               CRecv chan (Exp var singleTypeLabel),
                               CDeleteChan chan,
                               CBranch (Exp var singleTypeLabel) leftSeqs rightSeqs
                              ]
    restOfInstrs <- helper_ stype next role
    return (instrs Seq.>< restOfInstrs)
helper_ stype (Free (Select' receiver (exp :: Core (Either a b)) left right next)) role = do
    varEitherName <- freshVarName
    let chanKey = ChanKey { chanCreator = role , chanDestroyer = receiver}
    cid <- getChannelAndUpdateChanTable chanKey role
    let chan = Channel cid singleTypeLabel
    varLabelName <- freshVarName
    let varLabel = Var $ fromIntegral varLabelName :: Core Label
    varLeftVarName <- freshVarName
    varRightVarName <- freshVarName
    let varLeft = Var $ fromIntegral varLeftVarName :: Core a
    let varRight = Var $ fromIntegral varRightVarName :: Core b
    leftSeqs <- removeLastSeqSafe <$> helper_ singleType (left varLeft) role
    rightSeqs <- removeLastSeqSafe <$> helper_ singleType (right varRight) role
    updateEitherTypeCollects (singleType :: SingleType (Either a b))
    let instrs = Seq.fromList [CDecla varEitherName (singleType :: SingleType (Either a b)),
                               CAssgn varEitherName $ Exp exp singleType,
                               CDecla varLabelName singleTypeLabel,
                               CEither2Label varEitherName varLabelName,
                               CSend chan (Exp varLabel singleTypeLabel), 
                               CDecla varLeftVarName (singleType :: SingleType a),
                               CDecla varRightVarName (singleType :: SingleType b),
                               CSelect varEitherName varLeftVarName varRightVarName leftSeqs rightSeqs
                              ]
    restOfInstrs <- helper_ stype next role
    return (instrs Seq.>< restOfInstrs)

removeLastSeqSafe :: Seq a -> Seq a
removeLastSeqSafe x = case x of 
    Seq.Empty -> x
    xs Seq.:|> _ -> xs