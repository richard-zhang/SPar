{-# LANGUAGE FlexibleInstances #-}
module CodeGen.Utils where

import Language.C
import Data.String

cVar :: String -> CExpr
cVar str = CVar (internalIdent str) undefNode

cDeclr :: String -> CDeclr
cDeclr str = CDeclr (Just $ internalIdent str) [] Nothing [] undefNode

cInt :: Integer -> CExpr
cInt val = CConst (CIntConst (cInteger val) undefNode)

intTy :: CDeclSpec
intTy = CTypeSpec $ CIntType undefNode

voidTy  :: CDeclSpec
voidTy = CTypeSpec $ CVoidType undefNode

voidSpec  :: CTypeSpec
voidSpec = CVoidType undefNode

intSpec :: CTypeSpec
intSpec = CIntType undefNode

ty :: Ident -> CTypeSpec
ty = flip CTypeDef undefNode

(#) :: CExpr -> [CExpr] -> CExpr
f # args = CCall f args undefNode

(<--) :: CExpr -> CExpr -> CExpr
var <-- val = CAssign CAssignOp var val undefNode
infixl 3 <--

liftE :: CExpr -> CStat
liftE e = CExpr (Just e) undefNode

liftEToB :: CExpr -> CBlockItem
liftEToB = CBlockStmt . liftE

data UnOp = PlusPlus
          | MinusMinus
          | Minus
          | Plus
          | Not
          | Addr -- ^ The address of operator @&@.
          | Ind -- ^ The dereferencing operator in C @*@.
          deriving(Eq, Show)

-- | Convert a 'UnOp' to the corresponding 'CUnaryOp'.
toCUnaryOp :: UnOp -> CUnaryOp
toCUnaryOp Minus = CMinOp
toCUnaryOp Plus  = CPlusOp
toCUnaryOp Not   = CNegOp
toCUnaryOp Addr  = CAdrOp
toCUnaryOp Ind   = CIndOp

pre   :: UnOp -> CExpr -> CExpr
PlusPlus   `pre` exp = CUnary CPreIncOp exp undefNode
MinusMinus `pre` exp = CUnary CPreDecOp exp undefNode
op         `pre` exp = CUnary (toCUnaryOp op) exp undefNode

cvoidReturn :: CStat
cvoidReturn = CReturn Nothing undefNode

creturn :: CExpr -> CStat
creturn = flip CReturn undefNode . Just

decl :: CDeclSpec       -- ^ The declaration specifier, usually this is a type
        -> CDeclr      -- ^ Equivalent to the name of the object being declared. Often this will
                       -- make use of the overloaded string instance for 'CDeclr's
        -> Maybe CExpr -- ^ The optional init expression
        -> CDecl
decl ty name exp = CDecl [ty] [(Just name, flip CInitExpr undefNode `fmap` exp, Nothing)] undefNode

ptr :: CDeclr -> CDeclr
ptr (CDeclr nm mods cstr attrs node) = CDeclr nm (CPtrDeclr [] undefNode : mods) cstr attrs node

fun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> CStat -> CFunDef
fun specs name args body = annotatedFun specs name args [] body

-- | Identical to fun except this annotates the list of attributes given
-- as a list of strings.
annotatedFun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> [String] -> CStat -> CFunDef
annotatedFun specs name args annots body = CFunDef specs decl [] body undefNode
  where decl = CDeclr (Just $ fromString name)
               [CFunDeclr (Right (fmap ($Nothing) args, False)) [] undefNode]
               Nothing attrs undefNode
        attrs :: [CAttr]
        attrs = map (\ s -> CAttr (fromString s) [] undefNode) annots

instance IsString Ident where
    fromString = internalIdent
instance IsString CExpr where
    fromString s = CVar (fromString s) undefNode
instance IsString CDeclr where
    fromString str = CDeclr (Just $ fromString str) [] Nothing [] undefNode
instance IsString CDecl where
    fromString str = CDecl [CTypeSpec (CTypeDef (fromString str) undefNode)] [] undefNode
instance IsString CTypeSpec where
    fromString = flip CTypeDef undefNode . fromString