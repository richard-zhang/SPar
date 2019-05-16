{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
module RtDef where

import           Control.Monad.Free
import qualified Control.Monad.Indexed.Free    as F
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Type.Natural       hiding ( type (*)
                                                , S
                                                )
import qualified Data.Typeable                 as T

import           Def                     hiding ( return
                                                , (>>)
                                                , (>>=)
                                                )
import           Language.Poly.Core
import           Type
import           Type.Reflection
import           TypeValue

type ProcessRT a = (ProcRT a, Nat)

data ProcRTF next where
    Send' :: (Serialise a)  => Nat -> Core a -> next -> ProcRTF next
    Recv' :: (Serialise a)  => Nat -> (Core a -> next) -> ProcRTF next
    Select' :: (Serialise a, Serialise b, Serialise c) => Nat -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> next -> ProcRTF next
    Branch' :: (Serialise c) => Nat -> ProcRT c -> ProcRT c -> next -> ProcRTF next

    SelectMult' :: (Serialise a, Serialise b, Serialise c) => [Nat] -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> (Core c -> next) -> ProcRTF next
    BranchCont' :: (Serialise c) => Nat -> ProcRT c -> ProcRT c -> (Core c -> next) -> ProcRTF next

    Rec' :: Integer -> next -> ProcRTF next
    Mu' :: Integer -> ProcRTF a

    ForcedEval' :: (Serialise a) => Core a -> (Core a -> next) -> ProcRTF next

instance Functor ProcRTF where
    fmap f (Send' r v n              ) = Send' r v $ f n
    fmap f (Recv' r cont             ) = Recv' r (f . cont)
    fmap f (Select' r v cont1 cont2 n) = Select' r v cont1 cont2 (f n)
    fmap f (Branch' r left right n   ) = Branch' r left right (f n)

    fmap f (SelectMult' rs v cont1 cont2 cont) =
        SelectMult' rs v cont1 cont2 (f . cont)
    fmap f (BranchCont' r left right cont) =
        BranchCont' r left right (f . cont)

    fmap f (Rec' var n) = Rec' var (f n)
    fmap _ (Mu' var   ) = Mu' var
    fmap f (ForcedEval' val cont) = ForcedEval' val (f . cont)

-- type ProcRT a = Free ProcRTF (Core a)
type ProcRT a = ProcRT' (Core a)
type ProcRT' a = Free ProcRTF a

data AProcRT where
    AProcRT
        :: forall a. Serialise a
        => TypeRep a
        -> ProcRT a
        -> AProcRT

data AProcRTFunc a where
    AProcRTFunc
        :: forall a c. (Serialise a, Serialise c)
        => TypeRep c
        -> (Core a -> ProcRT c)
        -> AProcRTFunc a

type AProcessRT = (AProcRT, Nat)

toAProc :: Serialise a => ProcRT a -> AProcRT
toAProc (val :: ProcRT a) = AProcRT (typeRep :: TypeRep a) val

toAProcRTFunc
    :: (Serialise a, Serialise b) => (Core a -> ProcRT b) -> AProcRTFunc a
toAProcRTFunc (f :: Core a -> ProcRT b) = AProcRTFunc (typeRep :: TypeRep b) f

send' :: Serialise a => Nat -> Core a -> ProcRT a
send' role value = liftF $ Send' role value value

recv' :: Serialise a => Nat -> ProcRT a
recv' role = liftF $ Recv' role id

rec' :: Integer -> ProcRT a -> ProcRT a
rec' var body = Free $ Rec' var body

mu' :: Integer -> ProcRT a
mu' var = liftF $ Mu' var

select'
    :: (Serialise a, Serialise b, Serialise c)
    => Nat
    -> Core (Either a b)
    -> (Core a -> ProcRT c)
    -> (Core b -> ProcRT c)
    -> ProcRT ()
select' role var cont1 cont2 = liftF $ Select' role var cont1 cont2 (Lit ())

branch' :: Serialise c => Nat -> ProcRT c -> ProcRT c -> ProcRT ()
branch' role left right = liftF $ Branch' role left right (Lit ())

selectMulti'
    :: (Serialise a, Serialise b, Serialise c)
    => [Nat]
    -> Core (Either a b)
    -> (Core a -> ProcRT c)
    -> (Core b -> ProcRT c)
    -> ProcRT c
selectMulti' rs var cont1 cont2 = liftF $ SelectMult' rs var cont1 cont2 id

branchCont' :: Serialise c => Nat -> ProcRT c -> ProcRT c -> ProcRT c
branchCont' role left right = liftF $ BranchCont' role left right id

forcedEval' :: Serialise a => Core a -> ProcRT a
forcedEval' val = liftF $ ForcedEval' val id

convert' :: ProcRT a -> STypeV ()
convert' = convert 0

ignoreOutput :: ProcRT a -> ProcRT ()
ignoreOutput = (>> return (Lit ()))

convert :: Integer -> ProcRT a -> STypeV ()
convert _ (Pure a) = Pure ()
convert n (Free (Send' r v next)) =
    Free (S r (T.typeRep $ extractType v) $ convert n next)
convert n (Free (Recv' r cont)) = Free
    (R r (T.typeRep $ extractParamType cont) $ convert (n + 1) (cont $ Var n))
convert n (Free (Select' r v cont1 cont2 next)) = Free
    (Se r
        (convert 1 (cont1 $ Var 0))
        (convert 1 (cont2 $ Var 0))
        (convert n next)
    )
convert n (Free (Branch' r left right next)) =
    Free (B r (convert 0 left) (convert 0 right) (convert n next))
convert n (Free (BranchCont' r left right cont)) = Free
    (B r (convert 0 left) (convert 0 right) $ convert (n + 1) (cont $ Var n))

eraseSessionInfo' :: Proc' i j a -> ProcRT a
eraseSessionInfo' (F.Pure v) = Pure v
eraseSessionInfo' (F.Free (Send (r :: Sing (n :: Nat)) v next)) =
    Free (Send' (fromSing r) v (eraseSessionInfo' next))
eraseSessionInfo' (F.Free (Recv (r :: Sing (n :: Nat)) cont)) =
    Free (Recv' (fromSing r) (eraseSessionInfo' . cont))
eraseSessionInfo' (F.Free (Select (r :: Sing (n :: Nat)) v cont1 cont2 next)) =
    Free
        (Select' (fromSing r)
                 v
                 (eraseSessionInfo' . cont1)
                 (eraseSessionInfo' . cont2)
                 (eraseSessionInfo' next)
        )
eraseSessionInfo' (F.Free (Branch (r :: Sing (n :: Nat)) left right next)) =
    Free
        (Branch' (fromSing r)
                 (eraseSessionInfo' left)
                 (eraseSessionInfo' right)
                 (eraseSessionInfo' next)
        )

eraseSessionInfo :: Process k a -> ProcessRT a
eraseSessionInfo (Process n value) = (eraseSessionInfo' value, fromSing n)

convert2Normal :: PList xs -> [ProcessRT ()]
convert2Normal PNil         = []
convert2Normal (PCons p ps) = eraseSessionInfo p : convert2Normal ps

type Proc'' (i :: SType * *) a = Proc' i ( 'Pure ()) a

withProcRT
    :: Typeable a
    => ProcRT a
    -> (forall (n :: SType * *) . Sing n -> Proc n a -> r)
    -> r
withProcRT (Free (Send' n val next)) f = withProcRT next $ \info cont ->
    case toSing n of
        SomeSing role ->
            f (SSend role (extractType val) info) (F.Free (Send role val cont))
withProcRT (Free (Recv' n next)) f = withProcRT (next $ Var 0) $ \info cont ->
    case toSing n of
        SomeSing role -> f (SRecv role (extractParamType next) info)
                           (F.Free (Recv role (const cont)))
withProcRT (Pure v :: ProcRT a) f = f (SPure $ Proxy @a) (F.Pure v)

data SomeTypingInfo where
    SomeTypingInfo :: Sing (r :: Nat) -> Sing (n :: SType * *) -> SomeTypingInfo

data SomeProc a where
    SomeProc :: Sing n -> Proc n a -> SomeProc a

typeInferWithRole :: Typeable a => ProcessRT a -> SomeTypingInfo
typeInferWithRole (proc, role) =
    withSomeSing role (\r -> withProcRT proc (const . (SomeTypingInfo r)))

-- typeInferList :: Typeable a => [ProcessRT a] -> Bool
-- typeInferList procs = allTrue $ fmap helper $ handShake $ map typeInferWithRole procs
--     where
--         helper (SomeTypingInfo aid proca, SomeTypingInfo bid procb) =
--             (Type.dual (Type.project proca bid) aid) %~ (Type.project procb aid)
--         allTrue [] = True
--         allTrue (x : xs) = case x of
--             Proved _ -> allTrue xs
--             Disproved _ -> False

handShake :: [a] -> [(a, a)]
handShake []       = []
handShake (x : xs) = fmap ((,) x) xs ++ handShake xs

typeCheck :: Typeable a => ProcRT a -> Sing (n :: SType * *) -> Maybe (Sing n)
typeCheck proc providedType = case typeInfer proc of
    SomeSType inferedType -> case providedType %~ inferedType of
        Proved Refl -> Just providedType
        _           -> Nothing

typeInfer :: Typeable a => ProcRT a -> SomeSType
typeInfer proc = withProcRT proc (const . SomeSType)
