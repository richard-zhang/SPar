{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Example.Miscellaneous where
import Lib

cgt0 = do
    send' one (Lit 10 :: Core Int)
    x :: Core Int <- recv' one
    return x

cgt0' = do
    send' zero (Lit 10 :: Core Int)
    x :: Core Int <- recv' zero
    return x

cgt1 = do
    x :: Core Int <- recv' zero
    send' zero (Lit 20 :: Core Int)
    return x

cgt1' = do
    x :: Core Int <- recv' one
    send' one (Lit 20 :: Core Int)
    return x

bad = do
    x :: Core Int <- recv' zero
    bad

good :: ProcRT ()
good = rec' 0 $ do
    x :: Core Int <- recv' zero
    mu' 0

good' :: ProcRT ()
good' = rec' 0 $ do
    send' one (Lit 20 :: Core Int)
    mu' 0

cgts3 = [(good', zero), (good, one)]

recTest = rec' 0 $ branch'
    zero
    (do
        send' zero (Lit 1 :: Core Int)
--        send' zero (Lit 2 :: Core Int)
--        send' zero (Lit 3 :: Core Int)
    )
    (send' zero (Lit 99 :: Core Int) >> mu' 0)

recTest' = select'
    one
    (Lit (Right () :: Either () ()))
    (\_ -> return (Lit ()))
    (\_ -> do
        x :: Core Int <- recv' one
        select'
            one
            (Lit (Left () :: Either () ()))
            (\_ -> do
                y :: Core Int <- recv' one
--                z :: Core Int <- recv' one
--                q :: Core Int <- recv' one
                return (Lit ())
            )
            (\_ -> return (Lit ()))
    )

cgts4 = [(recTest', zero), (recTest, one)]

-- zero
branchCont =
    (branchCont'
            one
            (do
                y :: Core Int <- recv' one
                return y
            )
            (do
                y :: Core Int <- recv' one
                return y
            )
        )
        >>= (\x -> send' one x)
-- one
coBranchCont =
    (select' zero
                (Lit (Left () :: Either () ()))
                (\_ -> send' zero (Lit (3 :: Int)))
                (\_ -> send' zero (Lit (5 :: Int)))
        )
        >>= (\_ -> recv' zero :: ProcRT Int)
cgts5 = [(branchCont, zero), (coBranchCont, one)]

-- Select broadcast
headProc = selectMulti' [one, two, three]
                        (Lit (Left () :: Either () ()))
                        (\_ -> return (Lit ()))
                        (\_ -> return (Lit ()))
oneProc = branch'
    zero
    (ignoreOutput $ do
        y :: Core Int <- recv' two
        return y
    )
    (ignoreOutput $ do
        y :: Core Int <- recv' three
        return y
    )
twoProc = branch'
    zero
    (ignoreOutput $ do
        send' one (Lit (3 :: Int))
        x :: Core Int <- recv' three
        return x
    )
    (ignoreOutput $ return (Lit ()))
threeProc = branch'
    zero
    (ignoreOutput $ do
        send' two (Lit (4 :: Int))
    )
    (ignoreOutput $ do
        send' one (Lit (3 :: Int))
    )
cgts6 = [(headProc, zero), (oneProc, one), (twoProc, two), (threeProc, three)]

-- test nested type
nestedBranch = branch' one cgt0 cgt1'
nestedSelectOne = select'
    zero
    (Lit (Right (Left ()) :: Either () (Either () ())))
    (\_ -> cgt1)
    (\_ -> cgt0')
nestedSelectTwo = select' zero
                            (Lit (Left () :: Either () (Either () ())))
                            (\_ -> cgt1)
                            (\_ -> cgt0')

cgts7 = [(nestedBranch, zero), (nestedSelectOne, one)]
cgts8 = [(nestedBranch, zero), (nestedSelectTwo, one)]

cgb = branch' one cgt0 cgt1'
cgs = select' zero (Lit (Left () :: Either () ())) (\_ -> cgt1) (\_ -> cgt0')

cgts1 = [(cgt0, zero), (cgt1, one)]
cgts1' = [(ignoreOutput cgt0, zero), (ignoreOutput cgt1, one)]
cgts2 = [(cgb, zero), (cgs, one)]

procList :: ProcRT [Int]
procList = send' one (Lit [1, 2, 3, 4])

procListRecv :: ProcRT [Int]
procListRecv = recv' zero

cgts9 = [(procList, zero), (procListRecv, one)]

cgts10 =
    [ ((send' one (Pair (Lit 3) (Lit 4))) :: ProcRT (Int, Int), zero)
    , ((recv' zero) :: ProcRT (Int, Int)                      , one)
    ]

cgts11 =
    [ ( (send' one (Pair (Pair (Lit 2) (Lit 1)) (Lit 4))) :: ProcRT
                ((Int, Int), Int)
        , zero
        )
    , ((recv' zero) :: ProcRT ((Int, Int), Int), one)
    ]

testFunc :: Core (Int -> Int)
testFunc = Prim "testing" undefined

funcapts = [
    (do
        send' one (Lit 3 :: Core Int)
        y :: Core Int <- recv' one
        return (Lit ()), zero),
    ((recv' zero :: ProcRT Int) >>= (return . (testFunc :$)) >>= send' zero >>= \_ -> return (Lit ()), one)
    ]