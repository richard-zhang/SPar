{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Poly.Core2
    ( Core(..)
    , Serialise
    , extractType
    , extractParamType
    , interp
    , showDebug
    , getNodesCore
    , splitTree
    )
where
import           CodeGen.Type
import           Data.Proxy
import           Language.Poly.Nat
import           Type.Reflection
import           Data.Type.Natural.Class.Order
import           Proof.Propositional
import           Data.Singletons.Prelude.Enum

infixr 3 :***
infixr 3 :&&&
infixr 1 :>>>

data Core a where
    Lit :: a -> Core a
    Var :: Integer -> Core a

    Prim  :: String -- XXX: name of the "primitive function"
             -> a -- Semantics in Haskell of the primitive function
             -> Core a

    (:$) :: (Repr a, Repr b) =>  Core (a -> b) -> Core a -> Core b

    Id :: Core (a -> a)
    Const :: Core a -> Core (b -> a)
    Unit :: Core ()

    Fst :: (Repr b) => Core ((a, b) -> a)
    Snd :: (Repr a) => Core ((a, b) -> b)
    Pair :: Core a -> Core b -> Core (a, b)

    Inl :: Core (a -> Either a b)
    Inr :: Core (b -> Either a b)

    Swap :: Core (((a,b), (c, d)) -> ((a, c), (b, d)))

    (:&&&) :: (Serialise a, Serialise b, Serialise c) => Core (a -> b) -> Core (a -> c) -> Core (a -> (b, c))
    (:***) :: (Serialise a, Serialise b, Serialise c, Serialise d) => Core (a -> c) -> Core (b -> d) -> Core ((a, b) -> (c, d))
    (:>>>) :: (Serialise b) => Core (a -> b) -> Core (b -> c) -> Core (a -> c)

    ZipWithTree :: (Serialise a, Serialise b) => SNat n -> Core ((a, a) -> b) -> Core ((Tree ('S n) a) -> (Tree n b))

    Lift :: (Serialise a, Serialise b) => (Core a -> Core b) -> Core (a -> b)

showDebug :: Core a -> String
showDebug (x :&&& y) =
    "(" ++ showDebug x ++ ")" ++ " &&& " ++ "(" ++ showDebug y ++ ")"
showDebug (x :*** y) =
    "(" ++ showDebug x ++ ")" ++ " >>> " ++ "(" ++ showDebug y ++ ")"
showDebug (x :>>> y) =
    "(" ++ showDebug x ++ ")" ++ " *** " ++ "(" ++ showDebug y ++ ")"
showDebug (Swap           :$ y     ) = "swap " ++ showDebug y
showDebug ((Prim ident _) :$ subExp) = ident
showDebug (Snd            :$ subExp) = "Snd (" ++ showDebug subExp ++ ")"
showDebug (Fst            :$ subExp) = "Fst (" ++ showDebug subExp ++ ")"
showDebug (Id             :$ subExp) = "Id (" ++ showDebug subExp ++ ")"
showDebug (Inl            :$ subExp) = "Inl (" ++ showDebug subExp ++ ")"
showDebug (Inr            :$ subExp) = "Inr (" ++ showDebug subExp ++ ")"
showDebug (Const a        :$ subExp) = "Const (" ++ showDebug subExp ++ ")"
showDebug (x              :$ subExp) = showDebug x
showDebug (Unit                    ) = "unit"
showDebug (Var _                   ) = "Var"
showDebug (Pair x y) = "(" ++ showDebug x ++ "," ++ showDebug y ++ ")"
showDebug (Lit x                   ) = ""
showDebug (Fst                     ) = "WFst"
showDebug (Snd                     ) = "WSnd"
showDebug (Id                      ) = "WId"
showDebug (Const _                 ) = "WConst"
showDebug (Inl                     ) = "WInl"
showDebug (Inr                     ) = "WInr"
showDebug (Swap                    ) = "WSwap"
showDebug (Prim ident _            ) = "W" ++ ident


extractType :: Core a -> Proxy a
extractType _ = Proxy

extractParamType :: (Core a -> b) -> Proxy a
extractParamType _ = Proxy

interp :: Core t -> t
interp Unit    = ()
interp (Lit x) = x

getNodesCore
    :: (Serialise a) => Proxy a -> SNat n -> Core (Tree n a) -> [Core a]
getNodesCore proxy n x = case minusNilpotent n of
    Refl -> getLevelsCore (leqRefl n) proxy n n x

getLevelsCore
    :: (Serialise a)
    => IsTrue (m <= n)
    -> Proxy a
    -> SNat m
    -> SNat n
    -> Core (Tree n a)
    -> [Core (Tree (n - m) a)]
getLevelsCore _       _     SZ     _ x = [x]
getLevelsCore witness proxy (SS m) n x = concat
    $ fmap (getterCore firstProof proxy m n) y
  where
    firstProof     = succLeqToLT m n witness
    secondWitnesss = ltToLeq m n firstProof
    y              = getLevelsCore secondWitnesss proxy m n x

getterCore
    :: (Serialise a)
    => Compare m n :~: 'LT
    -> Proxy a
    -> SNat m
    -> SNat n
    -> Core (Tree (n - m) a)
    -> [Core (Tree (n - ( 'S m)) a)]
getterCore comp proxy m n x = case pred2 comp m n of
    Refl -> helper gtZeroProof proxy snum x
  where
    snum        = sMinus comp n m
    gtZeroProof = gtZero comp m n

    helper
        :: (Serialise a)
        => IsTrue (Zero < n)
        -> Proxy a
        -> SNat n
        -> Core (Tree n a)
        -> [Core (Tree (Pred n) a)]
    helper witness proxy a@(SS n1) x = case getSDict n1 proxy of
        SDict -> [Fst :$ x, Snd :$ x]

splitTree
    :: (Serialise a)
    => SNat n
    -> Proxy a
    -> Core ((Tree n (a, a)) -> (Tree n a, Tree n a))
splitTree n (proxy :: Proxy a) =
    case (getSDict n (Proxy :: Proxy (a, a)), getSDict n proxy) of
        (SDict, SDict) -> Lift $ splitTree' n proxy

splitTree'
    :: (Serialise a)
    => SNat n
    -> Proxy a
    -> Core (Tree n (a, a))
    -> Core (Tree n a, Tree n a)
splitTree' SZ        _proxy             x = x
splitTree' n@(SS n1) (proxy :: Proxy a) x = Pair leftTree rightTree
  where
    left = case proofA of
        SDict -> splitTree' n1 proxy (Fst :$ x)
    right = case proofA of
        SDict -> splitTree' n1 proxy (Snd :$ x)
    leftTree = case proofB of
        SDict -> Pair (Fst :$ left) (Fst :$ right)
    rightTree = case proofB of
        SDict -> Pair (Snd :$ left) (Snd :$ right)
    proofA    = getSDict n1 pairProxy
    proofB    = getSDict n1 proxy
    pairProxy = (Proxy :: Proxy (a, a))
