{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Language.Poly.Core2
    ( Core(..)
    , Serialise
    , extractType
    , extractParamType
    , interp
    , showDebug
    )
where
import           CodeGen.Type
import           Data.Proxy
import           Language.Poly.Nat

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

    ZipWithTree :: (Serialise a) => SNat n -> Core ((a, a) -> a) -> Core ((Tree ('S n) a) -> a)

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
