{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Example.Divideconquer where
import Lib

helloWorld3 :: [ProcessRT ()]
helloWorld3 =
    runPipe' (compose' (helloWorld2 1 0) (helloWorld2 1 0)) (Lit 10 :: Core Int)

helloWorld2 :: Int -> Int -> Pipe Int Int
helloWorld2 y x = forkJoinDc2 x y dvd comb basic pre post
  where
    dvd source left right = do
        val :: Core Int <- recv' source
        _               <- send' left val
        _               <- send' right val
        return (Lit ())
    comb target left right = do
        val :: Core Int <- recv' left
        _y :: Core Int  <- recv' right
        _               <- send' target val
        return (Lit ())
    basic source target = do
        val :: Core Int <- recv' source
        _               <- send' target val
        return (Lit ())
    pre left right val = do
        _ <- send' left val
        _ <- send' right val
        return (Lit ())
    post left right = do
        val :: Core Int <- recv' left
        _ :: Core Int   <- recv' right
        return val