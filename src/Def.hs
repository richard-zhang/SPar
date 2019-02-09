{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module Def where

import           Control.Monad.Free
import           Control.Monad.State
import           Expr

type RoleID = Integer

data ProcF next where
    Send :: RoleID -> Expr a -> next -> ProcF next
    Receive :: RoleID -> (Expr a -> next) -> ProcF next
    Branch :: RoleID -> next -> next -> ProcF next
    Select :: [RoleID] -> Expr (Either a b) -> (Expr a -> next) -> (Expr b -> next) -> ProcF next

instance Functor ProcF where
    fmap f (Send r e n)             = Send r e $ f n
    fmap f (Receive r cont)         = Receive r (f . cont)
    fmap f (Branch r a b)           = Branch r (f a) (f b)
    fmap f (Select r e cont1 cont2) = Select r e (f . cont1) (f . cont2)

type Proc a = Free ProcF (Expr a)

send :: RoleID -> Expr a -> Proc a
send role value = Free $ Send role value (Pure value)

receive :: RoleID -> Proc a
receive role = Free $ Receive role Pure

select :: [RoleID] -> Expr (Either a b) -> (Expr a -> Proc c) -> (Expr b -> Proc c) -> Proc c
select roles expr fac fbc = Free $ Select roles expr fac fbc

branch :: RoleID -> Proc c -> Proc c -> Proc c
branch role lproc rproc = Free $ Branch role lproc rproc

end :: Proc ()
end = return Unit

data Trace =
    Seq String Trace
  | Node String Trace Trace
  | End
  deriving (Show, Eq)

traceHelper :: Proc a -> State Integer Trace
traceHelper (Pure _) = return End
traceHelper (Free (Send r e n)) = do
    traces <- traceHelper n
    return $ Seq ("Send to " ++ show r) traces
traceHelper (Free (Receive r cont)) = do
    v <- get
    put (v+1)
    traces <- traceHelper $ cont $ Var v
    return $ Seq ("Receive from " ++ show r) traces
traceHelper (Free (Select roles v fac fbc)) = do
    v <- get
    put (v+1)
    left <- traceHelper $ fac $ Var v
    right <- traceHelper $ fbc $ Var v
    return $ Node ("Select to " ++ show roles) left right
traceHelper (Free (Branch r lproc rproc)) = do
    left <- traceHelper lproc
    right <- traceHelper rproc
    return $ Node ("Branch from " ++ show r) left right

trace :: Proc a -> Trace
trace proc = evalState (traceHelper proc) 0