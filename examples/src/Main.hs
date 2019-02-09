module Main where

import           SPar
import Language.Poly

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

len :: Core [a] -> Core Int
len list = Prim "length" length :$ list

true :: Core (Either () ())
true = Inl :$ Unit

false :: Core (Either () ())
false = Inr :$ Unit

smaller :: Ord a => Core a -> Core a -> Core (Either () ())
smaller a b = Prim "<" small :$ a :$ a
    where small a b = if a > b then Left () else Right ()