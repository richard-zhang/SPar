{-# LANGUAGE GADTs #-}

module CodeGen.Data where

import Data.Char
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Type.Natural (Nat)
import Language.C
import Unsafe.Coerce

import CodeGen.Type
import CodeGen.Utils
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
  CEnd :: SingleType a -> Exp a -> Instr
  CDecla :: Int -> SingleType a -> Instr
  CAssgn :: Int -> Exp a -> Instr
  CBranch :: Exp a -> Seq Instr -> Seq Instr -> Instr
    -- the first int represents the value of either type
    -- the second int represents the variable consumed in the branch
  CSelect :: Int -> Int -> Int -> Seq Instr -> Seq Instr -> Instr
    -- the first int represents the variable of either type
    -- the second int represents the value of the label
  CEither2Label :: Int -> Int -> Instr

data ChanKey = ChanKey
  { chanCreator   :: Nat
  , chanDestroyer :: Nat
  } deriving (Eq, Ord)

convertToCExpr :: Exp a -> CExpr
convertToCExpr (Exp exp stype) =
  case exp of
    Lit x   -> CConst (CIntConst (cInteger (fromIntegral (unsafeCoerce x :: Int))) undefNode)
    Var num -> CVar (internalIdent $ "v" ++ show num) undefNode

stypeToCExpr :: SingleType a -> a -> CExpr
stypeToCExpr (NumSingleType numType) v = numTypeToCExpr numType v
stypeToCExpr LabelSingleType v =
  case v of
    Le -> cVar "LEFT"
    Ri -> cVar "RIGHT"
stypeToCExpr (UnionSingleType a b) v =
  case v of
    Left v1  -> helper "left" $ stypeToCExpr a v1
    Right v2 -> helper "right" $ stypeToCExpr b v2
  where
    helper str expr =
      empCompoundLit [([], initExp $ cVar $ fmap toUpper str), ([], initList [([memberDesig str], initExp expr)])]
stypeToCExpr UnitSingleType _ = CConst (CIntConst (cInteger 0) undefNode)

numTypeToCExpr :: NumType a -> a -> CExpr
numTypeToCExpr (IntegralNumType (TypeInt _)) x = CConst (CIntConst (cInteger (fromIntegral x)) undefNode)
numTypeToCExpr (FloatingNumType (TypeFloat _)) x = CConst (CFloatConst (cFloat x) undefNode)

pthreadCreate_ :: CExpr -> CExpr -> CExpr -> CExpr -> CExpr
pthreadCreate_ a b c d = (cVar "pthread_create") # [a, b, c, d]

pthreadCreate :: CExpr -> CExpr -> CExpr
pthreadCreate a b = (cVar "pthread_create") # [a, cVar "NULL", b, cVar "NULL"]

threadName :: Nat -> String
threadName role = "th" ++ (show $ fromEnum role)

declThread :: Nat -> CDecl
declThread role = decl (CTypeSpec pthreadSpec) (cDeclr $ threadName role) Nothing

runThread :: Nat -> CExpr
runThread role = pthreadCreate (pre Addr $ cVar $ threadName role) (cVar $ procName role)

chanInit :: CExpr
chanInit = (cVar "chan_init") # [cInt 1]

chanDispose :: CExpr -> CExpr
chanDispose a = cVar "chan_dispose" # [a]

chanSendInt :: CExpr -> CExpr -> CExpr
chanSendInt a b = cVar "chan_send_int" # [a, b]

chanRecvInt :: CExpr -> CExpr -> CExpr
chanRecvInt a b = cVar "chan_recv_int" # [a, b]

-- pthread_t th;
-- pthread_create(&th, NULL, ping, NULL);
chanName__ :: CID -> String
chanName__ = ("c" ++) . show

chanName_ :: Channel a -> String
chanName_ (Channel cid _) = chanName__ cid

chanName :: Channel a -> CExpr
chanName = cVar . chanName_

chanSpec :: CTypeSpec
chanSpec = ty $ internalIdent "chan_t"

pthreadSpec :: CTypeSpec
pthreadSpec = ty $ internalIdent "pthread_t"

chanDecl :: CID -> CDecl
chanDecl cid = decl (CTypeSpec chanSpec) (ptr $ cDeclr $ chanName__ cid) Nothing

varName_ :: Int -> String
varName_ a = "v" ++ show a

varName :: Int -> CExpr
varName = cVar . varName_

declAndRunThread :: Nat -> [CBlockItem]
declAndRunThread role = [CBlockDecl $ declThread role, liftEToB $ runThread role]

instrToCBlock :: Instr -> CBlockItem
instrToCBlock (CInitChan chan) = liftEToB $ (chanName chan) <-- chanInit
instrToCBlock (CDeleteChan chan) = liftEToB $ chanDispose (chanName chan)
instrToCBlock (CSend chan expr) = liftEToB $ chanSendInt (chanName chan) $ convertToCExpr expr
instrToCBlock (CRecv chan expr) = liftEToB $ chanRecvInt (chanName chan) $ pre Addr $ convertToCExpr expr
instrToCBlock (CDecla var stype) = CBlockDecl $ stypeToCDecl stype var
instrToCBlock (CAssgn x value) = liftEToB $ (varName x) <-- convertToCExpr value
instrToCBlock (CEnd _ _) = CBlockStmt cvoidReturn
instrToCBlock (CBranch exp left right) =
  CBlockStmt $ cifElse (convertToCExpr exp ==: cVar "LEFT") (instrsToS left) (instrsToS right)
instrToCBlock (CSelect x y z left right) =
  CBlockStmt $ cifElse ((varName x & "label") ==: cVar "LEFT") undefined undefined
  where
    leftPart = block $ (liftEToB $ (varName y) <-- varName x & "label" & "left") : (fmap instrToCBlock (toList left))
    rightPart = block $ (liftEToB $ (varName z) <-- varName x & "label" & "right") : (fmap instrToCBlock (toList right))
instrToCBlock (CEither2Label x y) = liftEToB $ (varName y) <-- (varName x & "label")

stypeToCDecl :: SingleType a -> Int -> CDecl
stypeToCDecl (NumSingleType (IntegralNumType (TypeInt _))) x = decl intTy (cDeclr (varName_ x)) Nothing
stypeToCDecl (NumSingleType (FloatingNumType (TypeFloat _))) x = decl floatTy (cDeclr (varName_ x)) Nothing
stypeToCDecl LabelSingleType x = decl labelTy (cDeclr (varName_ x)) Nothing

labelTy :: CDeclSpec
labelTy = defTy "Label"

instrsToS :: Seq Instr -> CStat
instrsToS xs = CCompound [] (fmap instrToCBlock (toList xs)) undefNode

chanDecls :: CID -> [CExtDecl]
chanDecls cid = fmap (CDeclExt . chanDecl) [1 .. cid - 1]

procName :: Nat -> String
procName name = "proc" ++ (show $ fromEnum name)

instrToFunc :: Nat -> Seq Instr -> CFunDef
instrToFunc role instrs = fun [voidTy] (procName role) [] (instrsToS instrs)

mainFuncStat :: CID -> [Nat] -> CStat
mainFuncStat cid roles =
  CCompound
    []
    (fmap (\chan -> liftEToB $ (cVar $ chanName__ chan) <-- chanInit) [1 .. cid - 1] ++
     (roles >>= declAndRunThread) ++ [CBlockStmt $ creturn $ cInt 0])
    undefNode

mainFunc :: CID -> [Nat] -> CFunDef
mainFunc cid roles = fun [intTy] "main" [] (mainFuncStat cid roles)

codeGenCombined :: [(Nat, Seq Instr)] -> CID -> CTranslUnit
codeGenCombined instrs cid = CTranslUnit (channelDecls ++ main ++ funcs) undefNode
  where
    roles = fmap fst instrs
    channelDecls = chanDecls cid
    main = [CFDefExt $ mainFunc cid roles]
    funcs = fmap (CFDefExt . uncurry instrToFunc) instrs