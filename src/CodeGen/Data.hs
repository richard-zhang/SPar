{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.Data where

import           Data.Char
import           Data.Foldable
import           Data.List                      ( intercalate )
import           Data.Sequence                  ( Seq )
import           Data.String
import           Data.Type.Natural              ( Nat )
import           Data.Typeable
import           Language.C
import           Unsafe.Coerce

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
  liftEToB $ chanSendInt (chanName chan) $ convertToCExpr expr
instrToCBlock (CRecv chan expr) =
  liftEToB $ chanRecvInt (chanName chan) $ pre Addr $ convertToCExpr expr
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
convertToCExpr (Exp exp stype) = case exp of
  Lit x        -> stypeToCExpr stype x
  Var num      -> CVar (internalIdent $ "v" ++ show num) undefNode
  (Inl :$ exp) -> unionCExp (Left $ Exp exp $ getLeftUnionType stype) stype
  (Inr :$ exp) -> unionCExp (Right $ Exp exp $ getRightUnionType stype) stype

stypeToCExpr :: SingleType a -> a -> CExpr
stypeToCExpr (NumSingleType numType) v = numTypeToCExpr numType v
stypeToCExpr LabelSingleType         v = case v of
  Le -> cVar "LEFT"
  Ri -> cVar "RIGHT"
stypeToCExpr (UnionSingleType a b) v = case v of
  Left  v1 -> helper "left" $ stypeToCExpr a v1
  Right v2 -> helper "right" $ stypeToCExpr b v2
 where
  -- empCompoundLit [([], initExp $ cVar $ fmap toUpper str), ([], initList [([memberDesig str], initExp expr)])]
  helper str expr = defCompoundLit
    defName
    [ ([], initExp $ cVar $ fmap toUpper str)
    , ([], initList [([memberDesig str], initExp expr)])
    ]
  defName = eitherName (stypeToTypeRep a) (stypeToTypeRep b)
stypeToCExpr UnitSingleType _ = CConst (CIntConst (cInteger 0) undefNode)
stypeToCExpr (ProductSingleType a b) v = defCompoundLit
  defName
  [ ([], initExp $ stypeToCExpr a (fst v))
  , ([], initExp $ stypeToCExpr b (snd v))
  ]
  where defName = productName (stypeToTypeRep a) (stypeToTypeRep b)

unionCExp :: Either (Exp a) (Exp b) -> SingleType (Either a b) -> CExpr
unionCExp exp (UnionSingleType sa sb) =
  let defName = eitherName (stypeToTypeRep sa) (stypeToTypeRep sb)
  in  case exp of
        Left expL -> defCompoundLit
          defName
          [ ([], initExp $ cVar "LEFT")
          , ( []
            , initList [([memberDesig "left"], initExp $ convertToCExpr expL)]
            )
          ]
        Right expR -> defCompoundLit
          defName
          [ ([], initExp $ cVar "RIGHT")
          , ( []
            , initList [([memberDesig "right"], initExp $ convertToCExpr expR)]
            )
          ]

getLeftUnionType :: SingleType (Either a b) -> SingleType a
getLeftUnionType (UnionSingleType sa sb) = sa

getRightUnionType :: SingleType (Either a b) -> SingleType b
getRightUnionType (UnionSingleType sa sb) = sb

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
chanDecl cid =
  decl (CTypeSpec chanSpec) (ptr $ cDeclr $ chanName__ cid) Nothing

varName_ :: Int -> String
varName_ a = "v" ++ show a

varName :: Int -> CExpr
varName = cVar . varName_

declAndRunThread :: Nat -> [CBlockItem]
declAndRunThread role =
  [CBlockDecl $ declThread role, liftEToB $ runThread role]

typeToTypeSpec :: TypeRep -> CTypeSpec
typeToTypeSpec x | x == (typeRep $ Proxy @Int)   = CIntType undefNode
                 | x == (typeRep $ Proxy @Float) = CFloatType undefNode
                 | x == (typeRep $ Proxy @())    = CIntType undefNode
                 | otherwise = idSpec $ eitherName (typeOf ()) (typeOf ())

stypeToCDecl :: SingleType a -> Int -> CDecl
stypeToCDecl (NumSingleType (IntegralNumType (TypeInt _))) x =
  decl intTy (cDeclr (varName_ x)) Nothing
stypeToCDecl (NumSingleType (FloatingNumType (TypeFloat _))) x =
  decl floatTy (cDeclr (varName_ x)) Nothing
stypeToCDecl LabelSingleType x = decl labelTy (cDeclr (varName_ x)) Nothing
stypeToCDecl UnitSingleType  x = decl intTy (cDeclr (varName_ x)) Nothing
stypeToCDecl (UnionSingleType a b) x =
  decl (eitherTy a b) (cDeclr (varName_ x)) Nothing
stypeToCDecl (ProductSingleType a b) x =
  decl (productTy a b) (cDeclr (varName_ x)) Nothing

eitherName :: TypeRep -> TypeRep -> String
eitherName x y = intercalate "_" ["Sum", showType x, showType y]

eitherTy_ :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> String
eitherTy_ (_ :: SingleType a) (_ :: SingleType b) =
  eitherName (typeOf (undefined :: a)) (typeOf (undefined :: b))

eitherTy
  :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> CDeclSpec
eitherTy a b = defTy $ eitherTy_ a b

productName :: TypeRep -> TypeRep -> String
productName x y = intercalate "_" ["Product", showType x, showType y]

productTy_ :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> String
productTy_ (_ :: SingleType a) (_ :: SingleType b) =
  productName (typeOf (undefined :: a)) (typeOf (undefined :: b))

productTy
  :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> CDeclSpec
productTy a b = defTy $ eitherTy_ a b

showType :: TypeRep -> String
showType x | x == (typeRep $ Proxy @()) = "unit"
           | otherwise = fmap (\x -> if x == ' ' then '_' else x) $ show x

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

eitherTypeDecl :: [(TypeRep, TypeRep)] -> [CExtDecl]
eitherTypeDecl = fmap (CDeclExt . uncurry taggedUnionStruct)

anoyValueUnion :: TypeRep -> TypeRep -> CDecl
anoyValueUnion left right = anoyUnion
  "value"
  [("left", typeToTypeSpec left), ("right", typeToTypeSpec right)]

taggedUnionStruct :: TypeRep -> TypeRep -> CDecl
taggedUnionStruct a b =
  csu2 CStructTag (eitherName a b) [labelField, anoyValueUnion a b]

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
