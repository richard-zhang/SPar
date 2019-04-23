{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes    #-}

module CodeGen.Data where

import           Data.Char
import           Data.Foldable
import           Data.List                      ( intercalate )
import           Data.Sequence                  ( Seq )
import           Data.String
import           Data.Type.Natural              ( Nat )
import           Data.Typeable
import           Language.C

import           CodeGen.Type
import           CodeGen.Utils
import           Language.Poly.Core

type CID = Int

data Channel a where
  Channel :: CID -> SingleType a -> Channel a

data Exp a where
  Exp :: Core a -> SingleType a -> Exp a

data Instr where
  CInitChan :: Channel a -> Instr
  CDeleteChan :: Channel a -> Instr
  CSend :: Channel a -> Exp a -> Instr
  -- the Assumption for CRecv is that Exp a is always the variable, which value will be written to
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
  CRec :: Nat -> Instr

instrToCBlock :: Instr -> CBlockItem
instrToCBlock (CInitChan   chan) = liftEToB $ (chanName chan) <-- chanInit
instrToCBlock (CDeleteChan chan) = liftEToB $ chanDispose (chanName chan)
instrToCBlock (CSend chan expr) =
  liftEToB $ chanActionGeneral True (chanName chan) expr
instrToCBlock (CRecv chan expr) =
  liftEToB $ chanActionGeneral False (chanName chan) expr
instrToCBlock (CDecla var stype) = CBlockDecl $ stypeToCDecl stype var
instrToCBlock (CAssgn x value) =
  liftEToB $ (varName x) <-- convertToCExpr value
instrToCBlock (CEnd _ _              ) = CBlockStmt cvoidReturn
instrToCBlock (CBranch exp left right) = CBlockStmt $ cifElse
  (convertToCExpr exp ==: cVar "LEFT")
  (instrsToS left)
  (instrsToS right)
instrToCBlock (CSelect x y z left right) = CBlockStmt
  $ cifElse ((varName x & "label") ==: cVar "LEFT") leftPart rightPart
 where
  leftPart =
    block
      $ (liftEToB $ (varName y) <-- varName x & "value" & "left")
      : (fmap instrToCBlock (toList left))
  rightPart =
    block
      $ (liftEToB $ (varName z) <-- varName x & "value" & "right")
      : (fmap instrToCBlock (toList right))
instrToCBlock (CEither2Label x y) =
  liftEToB $ (varName y) <-- (varName x & "label")
instrToCBlock (CRec role) = liftEToB $ cVar (procRTName role) # []

data ChanKey = ChanKey
  { chanCreator   :: Nat
  , chanDestroyer :: Nat
  } deriving (Eq, Ord)

 -- Lit x   -> CConst (CIntConst (cInteger (fromIntegral (unsafeCoerce x :: Int))) undefNode)
convertToCExpr :: Exp a -> CExpr
convertToCExpr (Exp (exp :: Core a) stype) = case exp of
  Lit x           -> stypeToCExpr stype x
  Var num         -> CVar (internalIdent $ "v" ++ show num) undefNode
  (Inl :$ subExp) -> sumCExp (Left $ Exp subExp $ getLeftSumType stype) stype -- for merge
  (Inr :$ subExp) -> sumCExp (Right $ Exp subExp $ getRightSumType stype) stype -- for merge
  (Fst :$ subExp) -> CMember
    (convertToCExpr $ Exp subExp (ProductSingleType stype singleType))
    (internalIdent "fst")
    False
    undefNode
  (Snd :$ subExp) -> CMember
    (convertToCExpr $ Exp subExp (ProductSingleType singleType stype))
    (internalIdent "snd")
    False
    undefNode
  Pair l r -> productCExp (Exp l $ getLeftProdType stype)
                          (Exp r $ getRightProdType stype)
                          stype
  ((Prim ident _) :$ subExp) ->
    cVar ident # [convertToCExpr (Exp subExp singleType)]

stypeToCExpr :: SingleType a -> a -> CExpr
stypeToCExpr (NumSingleType numType) v = numTypeToCExpr numType v
stypeToCExpr LabelSingleType         v = case v of
  Le -> cVar "LEFT"
  Ri -> cVar "RIGHT"
stypeToCExpr UnitSingleType        _ = CConst (CIntConst (cInteger 0) undefNode)
stypeToCExpr s@(SumSingleType a b) v = case v of
  Left  v1 -> helper "left" $ stypeToCExpr a v1
  Right v2 -> helper "right" $ stypeToCExpr b v2
 where
  -- empCompoundLit [([], initExp $ cVar $ fmap toUpper str), ([], initList [([memberDesig str], initExp expr)])]
  helper str expr = defCompoundLit
    (show s)
    [ ([], initExp $ cVar $ fmap toUpper str)
    , ([], initList [([memberDesig str], initExp expr)])
    ]
stypeToCExpr s@(ProductSingleType a b) v = defCompoundLit
  (show s)
  [ ([], initExp $ stypeToCExpr a (fst v))
  , ([], initExp $ stypeToCExpr b (snd v))
  ]
stypeToCExpr s@(ListSingleType a) v = CStatExpr combStat undefNode
 where
    -- statment = fmap (\x y -> (("tmp" !: x) <-- stypeToCExpr s y)) (zip ([0..] :: [Int]) v)
  comb       = zip ([0 ..] :: [Int]) v
  exprs      = fmap (\(x, y) -> (("tmp" !: x) <-- (stypeToCExpr a y))) comb
  mallocExpr = castTy
    ( malloc
    $ ((fromIntegral $ length v) * (sizeOfDecl $ ty2Decl (stypeToTypeSpec a)))
    )
    (idSpec $ show s)
  statements =
    CBlockDecl
        (decl (CTypeSpec $ stypeToTypeSpec s)
              (fromString "tmp")
              (Just mallocExpr)
        )
      : (  fmap CBlockStmt
        $  fmap (\x -> CExpr (Just x) undefNode) exprs
        ++ [CExpr (Just $ cVar "tmp") undefNode]
        )
  combStat = CCompound [] statements undefNode

sumCExp :: Either (Exp a) (Exp b) -> SingleType (Either a b) -> CExpr
sumCExp exp s@(SumSingleType sa sb) = case exp of
  Left expL -> defCompoundLit
    (show s)
    [ ([], initExp $ cVar "LEFT")
    , ([], initList [([memberDesig "left"], initExp $ convertToCExpr expL)])
    ]
  Right expR -> defCompoundLit
    (show s)
    [ ([], initExp $ cVar "RIGHT")
    , ([], initList [([memberDesig "right"], initExp $ convertToCExpr expR)])
    ]

productCExp :: Exp a -> Exp b -> SingleType (a, b) -> CExpr
productCExp a b s@(ProductSingleType sa sb) = defCompoundLit
  (show s)
  [([], initExp $ convertToCExpr a), ([], initExp $ convertToCExpr b)]

getLeftSumType :: SingleType (Either a b) -> SingleType a
getLeftSumType (SumSingleType sa sb) = sa

getRightSumType :: SingleType (Either a b) -> SingleType b
getRightSumType (SumSingleType sa sb) = sb

getLeftProdType :: SingleType (a, b) -> SingleType a
getLeftProdType (ProductSingleType a b) = a

getRightProdType :: SingleType (a, b) -> SingleType b
getRightProdType (ProductSingleType a b) = b

stypeToTypeRep :: Typeable a => SingleType a -> TypeRep
stypeToTypeRep (x :: SingleType a) = typeOf (undefined :: a)

numTypeToCExpr :: NumType a -> a -> CExpr
numTypeToCExpr (IntegralNumType (TypeInt _)) x =
  CConst (CIntConst (cInteger (fromIntegral x)) undefNode)
numTypeToCExpr (FloatingNumType (TypeFloat _)) x =
  CConst (CFloatConst (cFloat x) undefNode)

pthreadCreate_ :: CExpr -> CExpr -> CExpr -> CExpr -> CExpr
pthreadCreate_ a b c d = (cVar "pthread_create") # [a, b, c, d]

pthreadCreate :: CExpr -> CExpr -> CExpr
pthreadCreate a b = (cVar "pthread_create") # [a, cVar "NULL", b, cVar "NULL"]

pthreadJoin :: CExpr -> CExpr
pthreadJoin a = (cVar "pthread_join") # [a, cVar "NULL"]

threadName :: Nat -> String
threadName role = "th" ++ (show $ fromEnum role)

declThread :: Nat -> CDecl
declThread role =
  decl (CTypeSpec pthreadSpec) (cDeclr $ threadName role) Nothing

runThread :: Nat -> CExpr
runThread role =
  pthreadCreate (pre Addr $ cVar $ threadName role) (cVar $ procName role)

joinThread :: Nat -> CExpr
joinThread role = pthreadJoin $ cVar $ threadName role

chanInit :: CExpr
chanInit = (cVar "chan_init") # [cInt 1]

chanDispose :: CExpr -> CExpr
chanDispose a = cVar "chan_dispose" # [a]

chanActionGeneral :: Bool -> CExpr -> Exp a -> CExpr
chanActionGeneral isSend channel exp@(Exp b stype) = case stype of
  (NumSingleType (IntegralNumType _)) ->
    cVar (intercalate "_" ["chan", action, show stype])
      # [channel, cexpr]
  (NumSingleType (FloatingNumType _)) ->
    cVar (intercalate "_" ["chan", action, "double"])
      # [channel, cexpr]
  (LabelSingleType) ->
    cVar (intercalate "_" ["chan", action, "int"])
      # [channel, cexpr]
  (UnitSingleType) ->
    cVar (intercalate "_" ["chan", action, "int"])
      # [channel, cexpr]
  (ListSingleType stype) ->
    cVar (intercalate "_" ["chan", action]) # [channel, cexpr]
  _ ->
    cVar (intercalate "_" ["chan", action, "int"])
      # [channel, cexpr]
  where 
    action = if isSend then "send" else "recv"
    cexpr = if isSend then convertToCExpr exp else pre Addr $ convertToCExpr exp

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
chanDecl cid =
  decl (CTypeSpec chanSpec) (ptr $ cDeclr $ chanName__ cid) Nothing

varName_ :: Int -> String
varName_ a = "v" ++ show a

varName :: Int -> CExpr
varName = cVar . varName_

declAndRunThread :: Nat -> [CBlockItem]
declAndRunThread role =
  [CBlockDecl $ declThread role, liftEToB $ runThread role]

stypeToTypeSpec :: SingleType a -> CTypeSpec
stypeToTypeSpec stype = case stype of
  (NumSingleType (IntegralNumType _)) -> CIntType undefNode
  (NumSingleType (FloatingNumType _)) -> CFloatType undefNode
  UnitSingleType                      -> CIntType undefNode
  LabelSingleType                     -> idSpec "Label"
  _                                   -> idSpec $ show stype -- include product type, sum type and list type and possibly recursive type

stypeToCDecl :: SingleType a -> Int -> CDecl
stypeToCDecl (NumSingleType (IntegralNumType (TypeInt _))) x =
  decl intTy (cDeclr (varName_ x)) Nothing
stypeToCDecl (NumSingleType (FloatingNumType (TypeFloat _))) x =
  decl floatTy (cDeclr (varName_ x)) Nothing
stypeToCDecl LabelSingleType x = decl labelTy (cDeclr (varName_ x)) Nothing
stypeToCDecl UnitSingleType x = decl intTy (cDeclr (varName_ x)) Nothing
--- include product type, sum type and list type and possibly recursive type
stypeToCDecl s x = decl (compoundTy s) (cDeclr (varName_ x)) Nothing

compoundTy :: SingleType a -> CDeclSpec
compoundTy a = CTypeSpec $ stypeToTypeSpec a

labelTy :: CDeclSpec
labelTy = defTy "Label"

instrsToS :: Seq Instr -> CStat
instrsToS xs = CCompound [] (fmap instrToCBlock (toList xs)) undefNode

chanDecls :: CID -> [CExtDecl]
chanDecls cid = fmap (CDeclExt . chanDecl) [1 .. cid - 1]

procName :: Nat -> String
procName name = "proc" ++ (show $ fromEnum name)

procRTName :: Nat -> String
procRTName name = procName name ++ "Rt"

instrToFunc :: Nat -> Seq Instr -> CFunDef
instrToFunc role instrs = fun [voidTy] (procName role) [] (instrsToS instrs)

instrToFuncRt :: Nat -> Seq Instr -> CFunDef
instrToFuncRt role instrs =
  fun [voidTy] (procRTName role) [] (instrsToS instrs)

pthreadFunc :: Nat -> CFunDef
pthreadFunc role = funP
  [voidTy]
  (procName role)
  []
  (block
    [ liftEToB $ cVar (procName role ++ "Rt") # []
    , CBlockStmt $ creturn $ cVar "NULL"
    ]
  )

labelEnum :: CExtDecl
labelEnum = CDeclExt $ cenum "Label" ["LEFT", "RIGHT"]

labelField :: CDecl
labelField = CDecl [defTy "Label"]
                   [(Just $ fromString "label", Nothing, Nothing)]
                   undefNode

prodSumTypeDecl :: [ASingleType] -> [CExtDecl]
prodSumTypeDecl = fmap (CDeclExt . dataStructDecl)

dataStructDecl :: ASingleType -> CDecl
dataStructDecl (ASingleType stype@(SumSingleType a b)) = csu2
  CStructTag
  (show stype)
  [labelField, anoyValueUnion]
 where
  anoyValueUnion = anoyUnion
    "value"
    [("left", stypeToTypeSpec a), ("right", stypeToTypeSpec b)]
dataStructDecl (ASingleType s@(ProductSingleType a b)) = csu2
  CStructTag
  (show s)
  [ CDecl [CTypeSpec $ stypeToTypeSpec a]
          [(Just $ fromString "fst", Nothing, Nothing)]
          undefNode
  , CDecl [CTypeSpec $ stypeToTypeSpec b]
          [(Just $ fromString "snd", Nothing, Nothing)]
          undefNode
  ]
dataStructDecl (ASingleType s@(ListSingleType stype)) = CDecl
  [CStorageSpec $ CTypedef undefNode, CTypeSpec $ stypeToTypeSpec stype]
  [(Just $ ptr $ fromString $ show s, Nothing, Nothing)]
  undefNode

mainFuncStat :: CID -> [Nat] -> CStat
mainFuncStat cid roles = CCompound
  []
  (  fmap (\chan -> liftEToB $ (cVar $ chanName__ chan) <-- chanInit)
          [1 .. cid - 1]
  ++ (roles >>= declAndRunThread)
  ++ fmap (liftEToB . joinThread) roles
  ++ fmap (\chan -> liftEToB $ chanDispose (cVar $ chanName__ chan))
          [1 .. cid - 1]
  ++ [CBlockStmt $ creturn $ cInt 0]
  )
  undefNode

mainFunc :: CID -> [Nat] -> CFunDef
mainFunc cid roles = fun [intTy] "main" [] (mainFuncStat cid roles)
