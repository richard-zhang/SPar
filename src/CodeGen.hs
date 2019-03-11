{-# LANGUAGE TupleSections #-}
module CodeGen where
import Control.Monad.Free
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Type.Natural
import Language.C.Syntax.AST
import Unsafe.Coerce

import CodeGen.Data
import CodeGen.Monad
import CodeGen.Type
import Language.Poly.Core
import RtDef

testCodeGen :: [ProcessRT Int] -> CTranslUnit
testCodeGen xs = evalCodeGen $ traverseToCodeGen singleTypeInt xs

traverseToCodeGen :: Monad m => SingleType a -> [ProcessRT a] -> CodeGen m [(Nat, (Seq Instr))]
traverseToCodeGen stype = flip forM $ uncurry $ helper stype

helper :: Monad m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Nat, Seq Instr)
helper singType process role = fmap (role,) $ helper_ singType process role

helper_ :: Monad m => SingleType a -> ProcRT a -> Nat -> CodeGen m (Seq Instr)
helper_ stype (Pure exp) _ =  return $ Seq.singleton (CEnd stype (Exp exp stype))
helper_ stype (Free (Send' receiver exp next)) role = do
    let chanKey = ChanKey { chanCreator = role , chanDestroyer = receiver}
    cid <- getChannelAndUpdateChanTable chanKey role
    -- TODO use singleTypeInt for now, will replace with singleType gained by pattern matching
    let chan = Channel cid singleTypeInt
    -- let instrs = Seq.fromList [CInitChan chan, CSend chan (Exp (unsafeCoerce exp) singleTypeInt)]
    let instrs = Seq.fromList [CSend chan (Exp (unsafeCoerce exp) singleTypeInt)]
    restOfInstrs <- helper_ stype next role
    return (instrs Seq.>< restOfInstrs)
helper_ stype (Free (Recv' sender cont)) role = do
    let chanKey = ChanKey { chanCreator = sender, chanDestroyer = role}
    cid <- getChannelAndUpdateChanTable chanKey role
    let chan = Channel cid singleTypeInt
    varName <- freshVarName
    let var = Var (fromIntegral varName)
    -- TODO use singleTypeInt for now, will replace with singleType gained by pattern matching
    let instrs = Seq.fromList [CDecla varName singleTypeInt, CRecv chan (Exp (unsafeCoerce var) singleTypeInt), CDeleteChan chan]
    restOfInstrs <- helper_ stype (cont var) role
    return (instrs Seq.>< restOfInstrs)
