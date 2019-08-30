{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE GADTs        #-}
module CodeGen.Monad where
import           Control.Monad.State
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Sequence                  ( Seq )
import           Data.Type.Natural
import           Language.C              hiding ( setFlag )
import           System.IO.Unsafe

import           CodeGen.Data
import           CodeGen.Type
import           Language.Poly.Core2

data CodeGenState = CodeGenState
  {
    newChanTable :: Map ChanKey CID,
    flagTable :: Map ChanKey Nat, -- flag to indiciate which process is encountered first in the code generation
    varNext   :: Int,
    chanNext  :: CID,
    dataStructCollect :: Set ASingleType
  }

newtype CodeGen m a = CodeGen { runCodeGen :: StateT CodeGenState m a }
    deriving (Functor, Applicative, Monad, MonadState CodeGenState, MonadIO)

evalCodeGen :: CodeGen IO [(Nat, Seq Instr)] -> CTranslUnit
evalCodeGen ma = codeGenCombined instrs st
  where
    (instrs, st) = unsafePerformIO $ runStateT (runCodeGen ma) initCodeGenState

evalCodeGen1 :: EntryRole a b -> CodeGen IO [(Nat, Seq Instr)] -> CTranslUnit
evalCodeGen1 entry ma = codeGenCombinedWithEntry instrs st entry
  where
    (instrs, st) = unsafePerformIO $ runStateT (runCodeGen ma) initCodeGenState

evalCodeGen2
    :: CExtDecl
    -> EntryRole a b
    -> CodeGen IO [(Nat, Seq Instr)]
    -> (CTranslUnit, CTranslUnit)
evalCodeGen2 main entry ma = codeGenCombinedWithEntry2 main instrs st entry
  where
    (instrs, st) = unsafePerformIO $ runStateT (runCodeGen ma) initCodeGenState

codeGenDs :: CodeGen IO [(Nat, Seq Instr)] -> CTranslUnit
codeGenDs ma = undefined

codeGenCombinedWithEntry
    :: [(Nat, Seq Instr)] -> CodeGenState -> EntryRole a b -> CTranslUnit
codeGenCombinedWithEntry instrs st entry@EntryRole {..} = CTranslUnit
    (  [labelEnum]
    ++ sumTypeDecls
    ++ channelDecls
    ++ funcsRt
    ++ funcsCaller
    ++ entryFuncDecl
    ++ main
    )
    undefNode
  where
    cid          = chanNext st
    channelDecls = chanDecls cid
    roles        = fmap fst instrs
    sumTypeDecls =
        fmap (CDeclExt . dataStructDecl) $ Set.toList $ dataStructCollect st
    main = [CFDefExt $ emptyMain]
    entryFuncDecl =
        [CFDefExt $ entryFunc entry sendChanCid recvChanCid cid roles]
    funcsRt     = fmap (CFDefExt . uncurry instrToFuncRt) instrs
    funcsCaller = fmap (CFDefExt . pthreadFunc . fst) instrs
    sendChanCid =
        getCid ChanKey { chanCreator = entryRole, chanDestroyer = startRole }
    recvChanCid =
        getCid ChanKey { chanCreator = endRole, chanDestroyer = entryRole }
    getCid key = fromMaybe
        (error ("key not exists in chantable" ++ show key))
        (Map.lookup key $ newChanTable st)

codeGenCombinedWithEntry2
    :: CExtDecl
    -> [(Nat, Seq Instr)]
    -> CodeGenState
    -> EntryRole a b
    -> (CTranslUnit, CTranslUnit)
codeGenCombinedWithEntry2 main instrs st entry@EntryRole {..} =
    (mainSource, dataHeader)
  where
    mainSource = CTranslUnit
        (channelDecls ++ funcsRt ++ funcsCaller ++ entryFuncDecl ++ [main])
        undefNode
    dataHeader   = CTranslUnit (labelEnum : sumTypeDecls) undefNode

    cid          = chanNext st
    channelDecls = chanDecls cid
    roles        = fmap fst instrs
    sumTypeDecls =
        fmap (CDeclExt . dataStructDecl) $ Set.toList $ dataStructCollect st
    entryFuncDecl =
        [CFDefExt $ entryFunc entry sendChanCid recvChanCid cid roles]
    funcsRt     = fmap (CFDefExt . uncurry instrToFuncRt) instrs
    funcsCaller = fmap (CFDefExt . pthreadFunc . fst) instrs
    sendChanCid =
        getCid ChanKey { chanCreator = entryRole, chanDestroyer = startRole }
    recvChanCid =
        getCid ChanKey { chanCreator = endRole, chanDestroyer = entryRole }
    getCid key = fromMaybe
        (error ("key not exists in chantable" ++ show key))
        (Map.lookup key $ newChanTable st)

codeGenCombined :: [(Nat, Seq Instr)] -> CodeGenState -> CTranslUnit
codeGenCombined instrs st = CTranslUnit
    (  [labelEnum]
    ++ sumTypeDecls
    ++ channelDecls
    ++ funcsRt
    ++ funcsCaller
    ++ main
    )
    undefNode
  where
    cid          = chanNext st
    channelDecls = chanDecls cid
    roles        = fmap fst instrs
    sumTypeDecls =
        fmap (CDeclExt . dataStructDecl) $ Set.toList $ dataStructCollect st
    main        = [CFDefExt $ mainFunc cid roles]
    funcsRt     = fmap (CFDefExt . uncurry instrToFuncRt) instrs
    funcsCaller = fmap (CFDefExt . pthreadFunc . fst) instrs

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState { flagTable         = Map.empty
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

createAndAddChannel2 :: Monad m => ChanKey -> CodeGen m CID
createAndAddChannel2 key = freshChanName >>= helper
  where
    helper :: Monad m => CID -> CodeGen m CID
    helper value = state $ \s@CodeGenState {..} ->
        (value, s { newChanTable = Map.insert key value newChanTable })

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

getChannelAndUpdateChanTable2 :: Monad m => ChanKey -> Nat -> CodeGen m CID
getChannelAndUpdateChanTable2 key _ = do
    codeGenState <- get
    let chanTable = newChanTable codeGenState
    let result    = Map.lookup key chanTable
    case result of
        Just cid -> return cid
        Nothing  -> createAndAddChannel2 key

updateDataStructCollectFromCore
    :: (Repr a, Monad m) => Core a -> CodeGen m ()
updateDataStructCollectFromCore ((_func :: Core (a -> b)) :$ v) =
    updateDataStructCollect (singleType :: SingleType a)
        >> updateDataStructCollectFromCore v
updateDataStructCollectFromCore _ = return ()

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

getNewVar :: Monad m => CodeGen m (Int, Core a)
getNewVar = do
    varId <- freshVarName
    return (varId, Var $ fromIntegral varId)

getChannel :: (Monad m, Repr a) => Nat -> Nat -> CodeGen m (Channel a)
getChannel sender receiver = do
    let chanKey = ChanKey { chanCreator = sender, chanDestroyer = receiver }
    cid <- getChannelAndUpdateChanTable2 chanKey receiver
    return $ Channel cid singleType


getSendValueChanInstr
    :: Monad m => Exp a -> SingleType a -> Nat -> Nat -> CodeGen m Instr
getSendValueChanInstr expr sType sender receiver = do
    let chanKey = ChanKey { chanCreator = sender, chanDestroyer = receiver }
    cid <- getChannelAndUpdateChanTable2 chanKey sender
    let chan = Channel cid sType
    return $ CSend chan expr
