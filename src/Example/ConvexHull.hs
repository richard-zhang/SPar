module Example.ConvexHull where
import           Lib

type P = (Int, Int)

findSide :: Core ((P, (P, P)) -> Int)
findSide = Prim "findSide" undefined

lineDist :: Core ((P, (P, P)) -> Int)
lineDist = Prim "lineDist" undefined

quickHullBase :: Core (([P], (P, P)) -> [P])
quickHullBase = Prim "baseFunc" undefined

maxDistPoint :: ArrowPipe ([P], (P, P)) P
maxDistPoint = arr $ Prim "maxDistPoint" undefined

findMaxMin :: Core ([P] -> (P, P))
findMaxMin = Prim "findMaxMin" undefined

swapCH :: Core ((P, P) -> (P, P))
swapCH = Prim "swap" undefined

newPair :: ArrowPipe (P, ([P], (P, P))) (P, P)
newPair = (arr Id *** (arr Snd >>> arr Fst)) >>> arr swapCH
-- newPair = arr Prim "newPair" undefined
newPair2 :: ArrowPipe (P, ([P], (P, P))) (P, P)
newPair2 = arr Id *** (arr Snd >>> arr Snd)
-- newPair2 = arr Prim "newPair2" undefined

filterPoints :: ArrowPipe ([P], (P, P)) [P]
filterPoints = arr $ Prim "filterPoints" undefined

splitCH :: ArrowPipe ([P], (P, P)) (([P], (P, P)), ([P], (P, P)))
-- splitCH =
--     (maxDistPoint &&& arr Id)
--         >>> (   (   ((arr Snd >>> arr Fst) &&& newPair)
--                 >>> ((filterPoints &&& arr Snd))
--                 )
--             &&& (   ((arr Snd >>> arr Fst) &&& newPair2)
--                 >>> ((filterPoints &&& arr Snd))
--                 )
--             )
splitCH = arr $ Prim "split" undefined

mergeCH :: ArrowPipe ([P], [P]) [P]
mergeCH = arr $ Prim "merge" undefined

quickHull' :: Int -> ArrowPipe ([P], (P, P)) [P]
quickHull' 0 = arr quickHullBase
quickHull' n =
    splitCH >>> (quickHull' (n - 1) *** quickHull' (n - 1)) >>> mergeCH

quickHull :: Int -> ArrowPipe [P] [P]
quickHull n =
    (arr Id &&& arr findMaxMin)
        >>> ((quickHull' n) &&& ((arr Id *** arr swapCH) >>> quickHull' n))
        >>> mergeCH

testQuickHull = runPipe1 zero (quickHull 1)