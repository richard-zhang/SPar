{-# LANGUAGE GADTs #-}
module CodeGen.Data where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Type.Natural (Nat)
import Language.C.Syntax.AST
import Language.C
import Unsafe.Coerce

import CodeGen.Type
import Language.Poly.Core

type CID = Int

data Channel a where
    Channel :: CID -> SingleType a -> Channel a

data Exp a where
    Exp :: Core a -> SingleType a -> Exp a

data Instr where
    CInitChan :: Channel a -> Instr
    CDeleteChan :: Channel a -> Instr
    CSend :: Channel a -> Exp a -> Instr
    CRecv :: Channel a -> Exp a -> Instr
    CDecla :: Int -> SingleType a -> Instr
    CEnd :: SingleType a -> Exp a -> Instr

data ChanKey = ChanKey { chanCreator :: Nat, chanDestroyer :: Nat } deriving (Eq, Ord)

convertToCExpr :: Exp a -> CExpr
convertToCExpr (Exp exp _) =
    case exp of
        Lit x -> CConst (CIntConst (cInteger (fromIntegral (unsafeCoerce x :: Int))) undefNode) 
        Var num -> CVar (internalIdent $ "v" ++ show num) undefNode
