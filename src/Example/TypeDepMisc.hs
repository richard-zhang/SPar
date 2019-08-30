{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Example.TypeDepMisc where

import           Prelude                 hiding ( return
                                                , (>>)
                                                , (>>=)
                                                )
import           Def
import           Language.Poly.Core2
import           Data.Type.Natural              ( snat )

test = do
    _ <- send [snat|1|] (Lit 10 :: Core Int)
    x :: Core (Either () ()) <- recv [snat|1|]
    _ <- select [snat|2|]
                x
                (\_ -> recv [snat|2|])
                (\_ -> send [snat|2|] (Lit 30 :: Core Int))
    return Unit

test1 = do
    _x :: Core Int <- recv [snat|0|]
    _              <- send [snat|0|] (Lit (Left () :: Either () ()))
    return Unit

test2 = branch [snat|0|] (send [snat|0|] (Lit 20 :: Core Int)) (recv [snat|0|])

test3 = select [snat|2|]
               (Lit (Right () :: Either () ()))
               (\_ -> recv [snat|2|])
               (\_ -> send [snat|2|] (Lit 30 :: Core Int))

example1 = do
    x :: Core Int <- recv [snat|0|]
    send [snat|1|] x

p0 = Process [snat|0|] test
p1 = Process [snat|1|] test1
p2 = Process [snat|2|] test2
p3 = Process [snat|0|] test3
ps = PCons p0 (PCons p1 (PCons p2 PNil))

ps2 = PCons p2 (PCons p3 PNil)

cgTest0 = send [snat|1|] (Lit 10 :: Core Int)

cgTest1 = do
    x :: Core Int <- recv [snat|0|]
    return x

pcg0 = Process [snat|0|] cgTest0
pcg1 = Process [snat|1|] cgTest1

hello :: DualityCons xs => PList xs -> Int
hello _ = 1

gk :: Int
gk = hello ps
