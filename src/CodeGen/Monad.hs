{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE GADTs        #-}
module CodeGen.Monad where
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.IO.Class
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Type.Natural
import           Data.Typeable
import           Language.C              hiding ( setFlag )
import           System.IO.Unsafe

import           CodeGen.Data
import           CodeGen.Type

data CodeGenState = CodeGenState
  {
    chanTable :: Map ChanKey (Seq CID),
    flagTable :: Map ChanKey Nat, -- flag to indiciate which process is encountered first in the code generation
    varNext   :: Int,
    chanNext  :: CID,
    dataStructCollect :: Set ASingleType,
    newChanTable :: Map ChanKey CID
  }

newtype CodeGen m a = CodeGen { runCodeGen :: StateT CodeGenState m a }
    deriving (Functor, Applicative, Monad, MonadState CodeGenState, MonadIO)

-- evalCodeGen :: CodeGen Identity [(Nat, (Seq Instr))] -> CTranslUnit
-- evalCodeGen = evalCodeGenHelper runIdentity
evalCodeGen = evalCodeGenHelper unsafePerformIO

evalCodeGenHelper
    :: (forall a . m a -> a) -> CodeGen m [(Nat, (Seq Instr))] -> CTranslUnit
evalCodeGenHelper f ma = codeGenCombined instrs st
    where (instrs, st) = f $ runStateT (runCodeGen ma) initCodeGenState

codeGenCombined :: [(Nat, Seq Instr)] -> CodeGenState -> CTranslUnit
codeGenCombined instrs state = CTranslUnit
    (  [labelEnum]
    ++ sumTypeDecls
    ++ channelDecls
    ++ funcsRt
    ++ funcsCaller
    ++ main
    )
    undefNode
  where
    cid          = chanNext state
    channelDecls = chanDecls cid
    roles        = fmap fst instrs
    sumTypeDecls = prodSumTypeDecl $ Set.toList $ dataStructCollect state
    main         = [CFDefExt $ mainFunc cid roles]
    funcsRt      = fmap (CFDefExt . uncurry instrToFuncRt) instrs
    funcsCaller  = fmap (CFDefExt . pthreadFunc . fst) instrs

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState { chanTable         = Map.empty
                                , flagTable         = Map.empty
                                , varNext           = 0
                                , chanNext          = 1
                                , dataStructCollect = Set.empty
                                , newChanTable      = Map.empty
                                }

freshChanName :: Monad m => CodeGen m CID
freshChanName =
    state $ \s@CodeGenState {..} -> (chanNext, s { chanNext = chanNext + 1 })

freshVarName :: Monad m => CodeGen m Int
freshVarName =
    state $ \s@CodeGenState {..} -> (varNext, s { varNext = varNext + 1 })

addChanNameToChanTable :: Monad m => ChanKey -> CID -> CodeGen m CID
addChanNameToChanTable key value = state $ \s@CodeGenState {..} ->
    ( value
    , s
        { chanTable = Map.insertWith (flip (Seq.><))
                                     key
                                     (Seq.singleton value)
                                     chanTable
        }
    )

createAndAddChannel :: Monad m => ChanKey -> CodeGen m CID
createAndAddChannel key = freshChanName >>= addChanNameToChanTable key

createAndAddChannel2 :: Monad m => ChanKey -> CodeGen m CID
createAndAddChannel2 key = freshChanName >>= helper key
  where
    helper :: Monad m => ChanKey -> CID -> CodeGen m CID
    helper key value = state $ \s@CodeGenState {..} ->
        (value, s { newChanTable = Map.insert key value newChanTable })

getAndDropChannel :: Monad m => ChanKey -> CodeGen m CID
getAndDropChannel key = do
    codeGenState <- get
    let table = chanTable codeGenState
    -- TODO use pattern matching to get the first element
    let cid   = Seq.index (fromJust $ Map.lookup key table) 0
    put (codeGenState { chanTable = Map.update (Just . Seq.drop 1) key table })
    return cid

checkChannelListNull :: Monad m => ChanKey -> CodeGen m Bool
checkChannelListNull key = do
    codeGenState <- get
    let table = chanTable codeGenState
    case Map.lookup key table of
        Just xs -> return $ Seq.null xs
        Nothing -> return True

setFlag :: Monad m => ChanKey -> Nat -> CodeGen m Nat
setFlag key value = state $ \s@CodeGenState {..} ->
    (value, s { flagTable = Map.insert key value flagTable })

getFlagOrSetFlag :: Monad m => ChanKey -> Nat -> CodeGen m Nat
getFlagOrSetFlag key value = do
    codeGenState <- get
    let flag = Map.lookup key $ flagTable codeGenState
    case flag of
        Just a  -> return a
        Nothing -> setFlag key value

getChannelAndUpdateChanTable :: Monad m => ChanKey -> Nat -> CodeGen m CID
getChannelAndUpdateChanTable key role = do
    flag <- getFlagOrSetFlag key role
    cid  <- if flag == role
        then createAndAddChannel key
        else getAndDropChannel key
    return cid

getChannelAndUpdateChanTable2 :: Monad m => ChanKey -> Nat -> CodeGen m CID
getChannelAndUpdateChanTable2 key _ = do
    codeGenState <- get
    let map    = newChanTable codeGenState
    let result = Map.lookup key map
    case result of
        Just cid -> return cid
        Nothing  -> createAndAddChannel2 key

updateDataStructCollect :: Monad m => SingleType a -> CodeGen m ()
updateDataStructCollect stype = case stype of
    (SumSingleType a b) ->
        update >> updateDataStructCollect a >> updateDataStructCollect b
    (ProductSingleType a b) ->
        update >> updateDataStructCollect a >> updateDataStructCollect b
    (ListSingleType a) -> update >> updateDataStructCollect a
    _                  -> return ()
  where
    update :: Monad m => CodeGen m ()
    update = state $ \s@CodeGenState {..} ->
        ( ()
        , s
            { dataStructCollect = Set.insert (toASingleType stype)
                                             dataStructCollect
            }
        )

-- debugging purpose
getSeqChan :: Monad m => ChanKey -> CodeGen m (Seq CID)
getSeqChan key = do
    codeGenState <- get
    let maybeSeq = Map.lookup key $ chanTable codeGenState
    return $ case maybeSeq of
        Just seq -> seq
        Nothing  -> Seq.empty
