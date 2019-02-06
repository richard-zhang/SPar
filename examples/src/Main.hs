module Main where

import           SPar

main :: IO ()
main = print $ trace master

proc1 :: Proc ()
proc1 = do
    send 2 (Lit 100)
    a <- receive 2 :: Proc Integer
    send 2 a
    end

-- Example of fork join model

master :: Proc ()
master = do
    input <- receive 0 :: Proc [Integer]
    numOfWorkers <- receive 0 :: Proc Int
    select [1,2,3] (smaller (len input) numOfWorkers)
        (\_ -> do
            send 1 (Lit 100)
            end)
        (\_ -> do
            receive 2
            end)
    end