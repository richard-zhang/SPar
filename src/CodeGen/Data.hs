{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RankNTypes    #-}

module CodeGen.Data where

import           Data.Char
import           Data.Foldable
import           Data.List                      ( intercalate )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.String
import           Data.Type.Natural              ( Nat )
import           Language.C
import           Data.IORef
import           System.IO.Unsafe

import           CodeGen.Type
import           CodeGen.Utils
import           Language.Poly.Core2

type CID = Int

data EntryRole a b = EntryRole
  { entryRole :: Nat
  , startRole :: Nat
  , endRole :: Nat
  , startType :: SingleType a
  , endType :: SingleType b
  }

data Channel a where
  Channel :: CID -> SingleType a -> Channel a

data Exp a where
  Exp :: Core a -> SingleType a -> Exp a

data ChanKey = ChanKey
  { chanCreator   :: Nat
  , chanDestroyer :: Nat
  } deriving (Eq, Ord, Show)

data Instr where
  CInitChan :: Channel a -> Instr
  CDeleteChan :: Channel a -> Instr
  CSend :: Channel a -> Exp a -> Instr
  -- the Assumption for CRecv is that Exp a is always the variable, which value will be written to
  CRecv :: Channel a -> Exp a -> Instr
  CEnd :: Exp a -> Instr
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

entryRoleToInstr
  :: EntryRole a b -> Integer -> Integer -> CID -> CID -> Seq Instr
entryRoleToInstr EntryRole {..} sendVar recvVar sendCid recvCid = Seq.fromList
  [ CSend sendChan sendExp
  , CDecla (fromInteger recvVar) endType
  , CRecv recvChan recvExp
  ]
 where
  sendExp  = Exp (Var sendVar) startType
  recvExp  = Exp (Var recvVar) endType
  sendChan = Channel sendCid startType
  recvChan = Channel recvCid endType

instrToCBlock :: Instr -> CBlockItem
instrToCBlock (CInitChan   chan) = liftEToB $ (chanName chan) <-- chanInit
instrToCBlock (CDeleteChan chan) = liftEToB $ chanDispose (chanName chan)
instrToCBlock (CSend chan expr) =
  liftEToB $ chanActionGeneral True (chanName chan) expr
instrToCBlock (CRecv chan expr) = if getDebugFlag
  then tryAddDebugStat
  else recvStat
 where
  recvStat        = liftEToB $ chanActionGeneral False (chanName chan) expr
  debugStat       = liftEToB $ printDebug expr
  tryAddDebugStat = CBlockStmt $ CCompound [] [recvStat, debugStat] undefNode
instrToCBlock (CDecla var stype) = CBlockDecl $ stypeToCDecl stype var
instrToCBlock (CAssgn x value) =
  liftEToB $ (varName x) <-- convertToCExpr value
instrToCBlock (CEnd expr) =
  CBlockStmt (CExpr (Just $ convertToCExpr expr) undefNode)-- CBlockStmt cvoidReturn
-- CBlockStmt $ creturn $ convertToCExpr expr
instrToCBlock (CBranch expr left right) = CBlockStmt $ cifElse
  (convertToCExpr expr ==: cVar "LEFT")
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

chanActionGeneral :: Bool -> CExpr -> Exp a -> CExpr
chanActionGeneral isSend channel expr@(Exp _ stype) = case stype of
  (NumSingleType (IntegralNumType _)) -> f "int"
  (NumSingleType (FloatingNumType _)) -> f "double"
  (LabelSingleType                  ) -> f "int"
  (UnitSingleType                   ) -> f "int"
  (ListSingleType _                 ) -> buf_action
  (SumSingleType     _ _            ) -> buf_action
  (ProductSingleType _ _            ) -> buf_action
 where
  action  = if isSend then "send" else "recv"
  ptrExpr = pre Addr $ convertToCExpr expr
  cexpr   = if isSend then convertToCExpr expr else ptrExpr
  f []     = cVar (intercalate "_" ["chan", action]) # [channel, cexpr]
  f prefix = cVar (intercalate "_" ["chan", action, prefix]) # [channel, cexpr]
  buf_action =
    cVar (intercalate "_" ["chan", action, "buf"])
      # [channel, ptrExpr, sizeOfDecl $ ty2Decl $ stypeToTypeSpec stype]

isMultiParamFunction :: Core a -> Int
isMultiParamFunction ((Prim _ _) :$  _) = 1
isMultiParamFunction (x :$ _y) = if subCount >= 0 then 1 + subCount else subCount
    where
      subCount = isMultiParamFunction x
isMultiParamFunction _ = -1

getCoreArgList :: Core a -> [CExpr]
getCoreArgList ((Prim _ _) :$ param) = [convertToCExpr (Exp param singleType)]
getCoreArgList (x :$ param) =
  getCoreArgList x ++ [convertToCExpr (Exp param singleType)]
getCoreArgList _ = error "not param wierd"

getFuncName :: Core a -> String
getFuncName ((Prim x _) :$ _) = x
getFuncName (x          :$ _) = getFuncName x
getFuncName _                 = error "not multi-param function"

convertToCExpr :: Exp a -> CExpr
convertToCExpr (Exp a _) | isMultiParamFunction a > 1 = fromString name # argLists
  where
    name = getFuncName a
    argLists = getCoreArgList a
convertToCExpr (Exp (expr :: Core a) stype) = case expr of
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
  (Id      :$ subExp) -> convertToCExpr (Exp subExp stype)
  (Const a :$ _     ) -> convertToCExpr (Exp a stype)
  ((Prim ident _) :$ subExp) ->
    cVar ident # [convertToCExpr (Exp subExp singleType)]
  Pair l r -> productCExp (Exp l $ getLeftProdType stype)
                          (Exp r $ getRightProdType stype)
                          stype
  (x :$ subExp) ->
    convertToCExpr (Exp x singleType) # [convertToCExpr (Exp subExp singleType)]
  x -> error $ showDebug x

printDebug :: Exp a -> CExpr
printDebug = debugPrint . samplingCExpr

-- TODO
-- Get the label if union, Get the samplineExpr snd if prod
-- Get the firstelement if array
-- It can debug
samplingCExpr :: Exp a -> (CExpr, String)
samplingCExpr e@(Exp _ stype) = case stype of
  (NumSingleType (IntegralNumType _)) -> (cExpr, "%d")
  (NumSingleType (FloatingNumType _)) -> (cExpr, "%lf")
  (UnitSingleType                   ) -> (cExpr, "%d")
  (LabelSingleType                  ) -> (cExpr, "%d")
  (ProductSingleType _ _) ->
    (CMember cExpr (fromString "fst") False undefNode, "%d") -- TODO
  (SumSingleType _ _) -> (fromIntegral (99 :: Integer), "%d") --TODO
  (ListSingleType _ ) -> (cExpr .: "size", "%u") -- print the size TODO
  where cExpr = convertToCExpr e

debugPrint :: (CExpr, String) -> CExpr
debugPrint (value, formatter) =
  cVar "printf"
    # [ CConst (CStrConst (cString $ "recv " ++ formatter ++ "\n") undefNode)
      , value
      ]

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
-- stypeToCExpr s@(ListSingleType a) v = rhs
--  where
--   sizeExpr = (fromIntegral $ length v) :: CExpr
--   rhs      = defCompoundLit
--     (show s)
--     [([], initExp sizeExpr), ([], initExp $ fromString "tmp")]
-- stypeToCExpr s@(ListSingleType a) v = listExpr
--  where
--   arrayInit = initListExprs (fmap (stypeToCExpr a) v)
--   tmp       = CDecl [CTypeSpec $ stypeToTypeSpec a]
--                     [(Just $ arr $ fromString "tmp", Just arrayInit, Nothing)]
--                     undefNode
--   rhs = defCompoundLit
--     (show s)
--     [([], initExp sizeExpr), ([], initExp $ fromString "tmp")]
--   tmp2     = decl (CTypeSpec $ idSpec $ show s) (fromString "tmp2") (Just rhs)
--   sizeExpr = (fromIntegral $ length v) :: CExpr
--   statements =
--     [ CBlockDecl tmp
--     , CBlockDecl tmp2
--     , CBlockStmt $ CExpr (Just $ cVar "tmp2") undefNode
--     ]
--   combStat = CCompound [] statements undefNode
--   listExpr = CStatExpr combStat undefNode
stypeToCExpr s@(ListSingleType a) v = defCompoundLit
  (show s)
  [([], initExp sizeExpr), ([], initExp listExpr)]
 where
  sizeExpr   = (fromIntegral $ length v) :: CExpr
  listExpr   = CStatExpr combStat undefNode
  comb       = zip ([0 ..] :: [Int]) v
  exprs      = fmap (\(x, y) -> (("tmp" !: x) <-- (stypeToCExpr a y))) comb
  mallocExpr = castTo
    ( malloc
    $ ((fromIntegral $ length v) * (sizeOfDecl $ ty2Decl (stypeToTypeSpec a)))
    )
    (decl (CTypeSpec $ stypeToTypeSpec a) justPtr Nothing)
  statements =
    CBlockDecl
        (decl (CTypeSpec $ stypeToTypeSpec a)
              (ptr $ fromString "tmp")
              (Just mallocExpr)
        )
      : (  fmap CBlockStmt
        $  fmap (\x -> CExpr (Just x) undefNode) exprs
        ++ [CExpr (Just $ cVar "tmp") undefNode]
        )
  combStat = CCompound [] statements undefNode

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
-- dataStructDecl (ASingleType s@(ListSingleType stype)) = CDecl
--   [CStorageSpec $ CTypedef undefNode, CTypeSpec $ stypeToTypeSpec stype]
--   [(Just $ ptr $ fromString $ show s, Nothing, Nothing)]
dataStructDecl (ASingleType s@(ListSingleType stype)) = csu2
  CStructTag
  (show s)
  [ CDecl [CTypeSpec $ idSpec "size_t"]
          [(Just $ fromString "size", Nothing, Nothing)]
          undefNode
  , CDecl [CTypeSpec $ stypeToTypeSpec stype]
          [(Just $ ptr $ fromString $ fromString "value", Nothing, Nothing)]
          undefNode
  ]
dataStructDecl _ = undefined

sumCExp :: Either (Exp a) (Exp b) -> SingleType (Either a b) -> CExpr
sumCExp expr s@(SumSingleType _ _) = case expr of
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
sumCExp _ _ = undefined

productCExp :: Exp a -> Exp b -> SingleType (a, b) -> CExpr
productCExp a b s@(ProductSingleType _ _) = defCompoundLit
  (show s)
  [([], initExp $ convertToCExpr a), ([], initExp $ convertToCExpr b)]
productCExp _ _ _ = undefined

numTypeToCExpr :: NumType a -> a -> CExpr
numTypeToCExpr (IntegralNumType (TypeInt _)) x =
  CConst (CIntConst (cInteger (fromIntegral x)) undefNode)
numTypeToCExpr (FloatingNumType (TypeFloat _)) x =
  CConst (CFloatConst (cFloat x) undefNode)

mainFunc :: CID -> [Nat] -> CFunDef
mainFunc cid roles = fun [intTy] "main" [] mainFuncStat
 where
  mainFuncStat = CCompound
    []
    (funcBodyHelper cid roles [] [CBlockStmt $ creturn $ cInt 0])
    undefNode

emptyMain :: CFunDef
emptyMain = fun [intTy] "main" [] mainFuncStat
 where
  mainFuncStat =
    CCompound [] [CBlockStmt $ creturn $ fromIntegral (0 :: Integer)] undefNode

benchMain :: (Repr a) => a -> CExtDecl
benchMain (sourceData :: a) = CFDefExt
  $ fun [intTy] "main" [] (CCompound [] statements undefNode)
 where
  stype = singleType :: SingleType a
  start =
    decl doubleTy (fromString "start") (Just $ (fromString "get_time") # [])
  call = (fromString "proc0") # [(fromString "a")]
  debugPrint = fromString "debug" # [call]
  end = decl doubleTy (fromString "end") (Just $ (fromString "get_time") # [])
  printTime =
    (fromString "printf")
      # [ CConst (CStrConst (CString "%lf\n" False) undefNode)
        , cOp CSubOp (fromString "end") (fromString "start")
        ]
  ret = creturn 0
  statements =
    (fst $ sourceDataDeclStatements 1 "tmp" "a" stype sourceData)
      ++ [ CBlockDecl start
         , CBlockStmt $ CExpr (Just debugPrint) undefNode
         , CBlockDecl end
         , CBlockStmt $ CExpr (Just printTime) undefNode
         , CBlockStmt ret
         ]

sourceDataDeclStatements
  :: Int -> String -> String -> SingleType a -> a -> ([CBlockItem], Int)
sourceDataDeclStatements count tmpName outputName s@(ListSingleType a) v =
  ([CBlockDecl tmp, CBlockDecl varA], count)
 where
  arrayInit = initListExprs (fmap (stypeToCExpr a) v)
  tmp       = CDecl [CTypeSpec $ stypeToTypeSpec a]
                    [(Just $ arr $ fromString tmpName, Just arrayInit, Nothing)]
                    undefNode
  varA =
    decl (CTypeSpec $ stypeToTypeSpec s) (fromString outputName) (Just rhs)
  sizeExpr = (fromIntegral $ length v) :: CExpr
  rhs      = defCompoundLit
    (show s)
    [([], initExp sizeExpr), ([], initExp $ fromString tmpName)]
sourceDataDeclStatements count _tmpName outputName s@(ProductSingleType a b) v
  = (left ++ right ++ [CBlockDecl varA], rightCount)
 where
  varLeft  = "aLeft" ++ show count
  varRight = "aRight" ++ show count
  (left, leftCount) =
    sourceDataDeclStatements (count + 1) "tmpLeft" varLeft a (fst v)
  (right, rightCount) =
    sourceDataDeclStatements (leftCount + 1) "tmpRight" varRight b (snd v)
  varA =
    decl (CTypeSpec $ stypeToTypeSpec s) (fromString outputName) (Just rhs)
  rhs = defCompoundLit
    (show s)
    [([], initExp $ fromString varLeft), ([], initExp $ fromString varRight)]
sourceDataDeclStatements count _tmpName outputName s@(NumSingleType (IntegralNumType _)) v
  = ([CBlockDecl varA], count)
 where
  varA =
    decl (CTypeSpec intSpec) (fromString outputName) (Just $ stypeToCExpr s v)
sourceDataDeclStatements count _tmpName outputName s@(NumSingleType (FloatingNumType _)) v
  = ([CBlockDecl varA], count)
 where
  varA =
    decl (CTypeSpec floatSpec) (fromString outputName) (Just $ stypeToCExpr s v)
sourceDataDeclStatements _ _ _ _ _ = undefined

funcBodyHelper :: CID -> [Nat] -> [CBlockItem] -> [CBlockItem] -> [CBlockItem]
funcBodyHelper cid roles middle end =
  (  fmap (\chan -> liftEToB $ (cVar $ chanName__ chan) <-- chanInit)
          [1 .. cid - 1]
  ++ (roles >>= declAndRunThread)
  ++ middle
  ++ fmap (liftEToB . joinThread) roles
  ++ fmap (\chan -> liftEToB $ chanDispose (cVar $ chanName__ chan))
          [1 .. cid - 1]
  ++ end
  )

entryFunc :: EntryRole a b -> CID -> CID -> CID -> [Nat] -> CFunDef
entryFunc a@EntryRole {..} sendChan recvChan cid roles = fun
  [outputDeclSpec]
  (procName entryRole)
  [decl inputDeclSpec inputVarDeclr]
  funcStats
 where
  inputVarDeclr  = (fromString (varName_ $ fromIntegral sendVar)) :: CDeclr
  inputDeclSpec  = CTypeSpec $ stypeToTypeSpec startType
  outputDeclSpec = CTypeSpec $ stypeToTypeSpec endType
  midBlockItems  = fmap instrToCBlock $ toList $ entryRoleToInstr a
                                                                  sendVar
                                                                  recvVar
                                                                  sendChan
                                                                  recvChan

  endBlockItems   = [CBlockStmt $ creturn $ varName (fromIntegral recvVar)]
  wholeBlockItmes = funcBodyHelper cid roles midBlockItems endBlockItems
  funcStats       = CCompound [] wholeBlockItmes undefNode
  sendVar         = 0
  recvVar         = 1

getLeftSumType :: SingleType (Either a b) -> SingleType a
getLeftSumType (SumSingleType sa _) = sa
getLeftSumType _                    = undefined

getRightSumType :: SingleType (Either a b) -> SingleType b
getRightSumType (SumSingleType _ sb) = sb
getRightSumType _                    = undefined

getLeftProdType :: SingleType (a, b) -> SingleType a
getLeftProdType (ProductSingleType a _) = a
getLeftProdType _                       = undefined

getRightProdType :: SingleType (a, b) -> SingleType b
getRightProdType (ProductSingleType _ b) = b
getRightProdType _                       = undefined

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

swapChanKey :: ChanKey -> ChanKey
swapChanKey key =
  ChanKey { chanCreator = chanDestroyer key, chanDestroyer = chanCreator key }

---- HACK ZONE UNSAFE
debugFlag :: IORef Bool
debugFlag = unsafePerformIO (newIORef False)

makeTrue :: IO ()
makeTrue = writeIORef debugFlag True

getDebugFlag :: Bool
getDebugFlag = unsafePerformIO $ readIORef debugFlag
---- UNSAFE HACK ZONE
