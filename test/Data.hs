{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes    #-}
module Data where

import Control.Monad.Free
import Data.Type.Natural (Nat, nat, snat)
import Data.Typeable
import Type
import TypeValue
import Rt

role1 :: Nat
role1 = [nat|1|]

role2 :: Nat
role2 = [nat|2|]

role3 :: Nat
role3 = [nat|3|]

intType :: TypeRep
intType = typeRep (Proxy :: Proxy Int)

singleSend :: Nat -> STypeV ()
singleSend role = Free (S role intType (Pure ()))

end :: STypeV ()
end = Pure ()

proc1 :: STypeV ()
proc1 = Free (S 1 (typeOf "str") (Pure ()))

proc2 :: STypeV ()
proc2 = Free (S 1 (typeOf 'c') (Pure ()))

proc3 :: STypeV ()
proc3 = Free (R 1 (typeOf "str") (Pure ()))
