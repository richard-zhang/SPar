{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CodeGen.Monad where
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Type.Natural
import Data.Typeable
import Language.C hiding (setFlag)

import CodeGen.Data
import CodeGen.Type

data CodeGenState = CodeGenState
  {
    chanTable :: Map ChanKey (Seq CID),
    flagTable :: Map ChanKey Nat, -- flag to indiciate which process is encountered first in the code generation
    varNext   :: Int,
    chanNext  :: CID,
    eitherTypeCollects :: Set (TypeRep, TypeRep)
  }

newtype CodeGen m a = CodeGen { runCodeGen :: StateT CodeGenState m a }
    deriving (Functor, Applicative, Monad, MonadState CodeGenState)

evalCodeGen :: CodeGen Identity [(Nat, (Seq Instr))] -> CTranslUnit
evalCodeGen ma = codeGenCombined instrs st
    where 
        Identity (instrs, st) = runStateT (runCodeGen ma) initCodeGenState

codeGenCombined :: [(Nat, Seq Instr)] -> CodeGenState -> CTranslUnit
codeGenCombined instrs state = CTranslUnit ([labelEnum] ++ eitherTypeDecls ++ channelDecls ++ funcsRt ++ funcsCaller ++ main) undefNode
  where
    cid = chanNext state
    channelDecls = chanDecls cid
    roles = fmap fst instrs
    eitherTypeDecls = eitherTypeDecl $ Set.toList $ eitherTypeCollects state
    main = [CFDefExt $ mainFunc cid roles]
    funcsRt = fmap (CFDefExt . uncurry instrToFuncRt) instrs
    funcsCaller = fmap (CFDefExt . pthreadFunc . fst) instrs

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState { chanTable = Map.empty, flagTable = Map.empty, varNext = 0, chanNext = 1 , eitherTypeCollects = Set.empty}

freshChanName :: Monad m => CodeGen m CID
freshChanName = state $ \s@CodeGenState{..} -> (chanNext, s { chanNext = chanNext + 1})

freshVarName :: Monad m => CodeGen m Int
freshVarName = state $ \s@CodeGenState{..} -> (varNext, s { varNext = varNext + 1})

addChanNameToChanTable :: Monad m => ChanKey -> CID -> CodeGen m CID
addChanNameToChanTable key value = state $ \s@CodeGenState{..} -> (value, s { chanTable = Map.insertWith (flip (Seq.><)) key (Seq.singleton value) chanTable })

createAndAddChannel :: Monad m => ChanKey -> CodeGen m CID
createAndAddChannel key = freshChanName >>= addChanNameToChanTable key

getAndDropChannel :: Monad m => ChanKey -> CodeGen m CID
getAndDropChannel key = do
    codeGenState <- get
    let table = chanTable codeGenState
    let cid = Seq.index (fromJust $ Map.lookup key table) 0
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
setFlag key value = state $ \s@CodeGenState{..} -> (value, s {flagTable = Map.insert key value flagTable })

getFlagOrSetFlag:: Monad m => ChanKey -> Nat -> CodeGen m Nat
getFlagOrSetFlag key value = do
    codeGenState <- get
    let flag = Map.lookup key $ flagTable codeGenState
    case flag of
        Just a  -> return a
        Nothing -> setFlag key value

getChannelAndUpdateChanTable :: Monad m => ChanKey -> Nat -> CodeGen m CID
getChannelAndUpdateChanTable key role = do
    flag <- getFlagOrSetFlag key role
    cid <- if flag == role then createAndAddChannel key else getAndDropChannel key
    return cid

updateEitherTypeCollects :: (Typeable a, Typeable b, Monad m) => SingleType (Either a b) -> CodeGen m ()
updateEitherTypeCollects (x :: SingleType (Either a b)) =
    state $ \s@CodeGenState{..} -> ((), s { eitherTypeCollects = Set.insert elem eitherTypeCollects }) 
    where
        elem = (typeOf (undefined :: a), typeOf (undefined :: b))