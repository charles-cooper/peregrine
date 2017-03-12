{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Peregrine.Compiler (
  codegen,
) where

import qualified Language.C.Utils as C
import           Language.C.Utils as C (C(..))
import           Language.C.Lib as CL

import           Protocol
import           Protocol.Backend.C.Base as CP

import qualified Data.Set as Set
import           Data.Set as Set (Set(..))
import qualified Data.Map as Map
import           Data.Map as Map (Map(..))
import qualified Data.IntMap as IMap
import           Data.IntMap as IMap (IntMap(..))
import           Data.Sequence (Seq(..), (|>))

import           Data.Fix
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Arrow
import           Data.Function

import           Data.Maybe
import           Data.Monoid
import           Data.List (tails, intercalate)
import           Data.Foldable
import           Data.Tuple (swap)
import           Utils

import           Data.String
import           Data.String.Interpolate.IsString

import           Language.Peregrine.AST
import           Language.Peregrine.DSL

-- | TODO zonk this dependency
import qualified Protocol.Tmx.TAQ.C as TAQ (symbol)

type Struct = [(C.Type, C.Identifier)] -- rename to StructMembers

-- Flag representing whether a variable is used outside of
-- its own dependencies. If so it needs to get written to the
-- global state, otherwise it can be assigned to a tmp var.
data Storage = Temporary | Permanent
  deriving (Eq, Show)
           
permanent :: Storage -> Bool
permanent = (== Permanent)
           
temporary :: Storage -> Bool
temporary = (== Temporary)

storageRequirement :: Dependencies -> Storage
storageRequirement (Deps deps revdeps mustStore) = if
  | mustStore                     -> Permanent
  | revdeps `Set.isSubsetOf` deps -> Temporary
  | otherwise                     -> Permanent

type HandleCont = Message CField -> (Message CField -> C C.Code) -> C C.Code

runHandleCont :: [HandleCont] -> Message CField -> C C.Code
runHandleCont ks msg = go ks
  where    
    go ks = case ks of
      []   -> return []
      k:ks -> (k msg) (\_msg -> go ks)

data Dependencies = Deps
  { _deps      :: Set (Message CField)
  , _revdeps   :: Set (Message CField)
  -- ^ perhaps a better name for this is scope
  , _mustStore :: Bool
  }        
           
dempty :: Dependencies
dempty = Deps Set.empty Set.empty False

data CompInfo = CompInfo
  { src          :: C.Identifier -- ^ The raw identifier
  , ref          :: C.Exp        -- ^ The expression including context
  , ctype        :: C.Type       -- ^ The C Type
  , dependencies :: Dependencies -- ^ Set of messages which trigger an update
  , tmp          :: Bool         -- ^ Does not need to be written to storage
  , cleanup      :: HandleCont   -- ^ What to do at the end
  , handler      :: HandleCont   -- ^ Function to generate C code
  }        

data CompState = CompState
  -- In the future if these turn out to be not performant,
  -- we can hash or compress representation of the nodes
  { nodeInfo  :: IntMap CompInfo
  -- ^ The compilation info for each node
  , groups    :: Map IGroup (C.Identifier, CompInfo, Struct)
  -- ^ Map from groups to structs
  , nodeOrder :: Seq CompInfo
  -- ^ An ordered visiting of the nodes
  --   Include the Node so we can lookup its revdeps
  --   A nice refactor would just be a (Seq NodeID) ..
  --   would need logic to guarantee uniqueness of
  --   nodes and also reify ordinary nodes / groups
  }
          
data CompEnv = CompEnv
  { specification :: Proto CField
  -- ^ The specification for compiling the data types to C
  , nodeContext   :: Nodes IContext
  -- ^ Metadata about the nodes calculated from previous passes
  }

-- Type for the Peregrine compiler
-- Rename to 'PeregrineCompiler' or 'PeregrineMonad'
type PeregrineC a = StateT CompState (ReaderT CompEnv C) a
          
type AssocList a b = [(a, b)]
                     
fieldsMap :: Message a -> AssocList String (Field a)
fieldsMap msg = (_name &&& id) <$> _fields msg
                     
msgsMap :: Proto a -> AssocList String (Message a)
msgsMap proto = (_msgName &&& id) <$> _outgoingMessages proto
                     
resolveProjection :: Projection -> (Field CField, Message CField)
resolveProjection (Projection proto msgName fieldName) = (field, msg)
  where              
    msg   = lookup msgName (msgsMap proto)
      & fromMaybe
        (error [i|Message "${msgName}" not found in proto ${_namespace proto}|])
    field = lookup fieldName (fieldsMap msg)
      & fromMaybe
        (error [i|Field "${fieldName}" not found in message ${_msgName msg}|])

liftC :: C a -> PeregrineC a
liftC = lift . lift
           
genLocalId :: C.Identifier -> Context a -> PeregrineC C.Identifier
-- TODO: generate from a new state for every fence
genLocalId default_ ctx = liftC $ C.genId (C.getIdentifier (cnm cid))
  where    
    cid = maybe (C.getIdentifier default_) id (annotation ctx)

appendTopsort :: CompInfo -> PeregrineC ()
appendTopsort node = do
  modify $ \st -> st { nodeOrder = (nodeOrder st) |> node }

withCtx :: C.Exp -> C.Identifier -> C.Exp
withCtx ctx x = [i|${ctx}->${x}|]
           
compileOp :: BinOp -> C.Exp -> C.Exp -> C.Exp
compileOp op x y = parens $ case op of
  Add -> [i|${x} + ${y}|]
  Sub -> [i|${x} - ${y}|]
  Mul -> [i|${x} * ${y}|]
  Div -> [i|${x} / ${y}|]
  Gt  -> [i|${x} > ${y}|]
  Ge  -> [i|${x} >= ${y}|]
  Lt  -> [i|${x} < ${y}|]
  Le  -> [i|${x} <= ${y}|]
  Eq  -> [i|${x} == ${y}|]
  Max -> [i|${x} > ${y} ? ${x} : ${y}|]
  Min -> [i|${x} < ${y} ? ${x} : ${y}|]
  where    
    parens e = "(" <> e <> ")"

topState :: String -- Identifier
topState = "state"

topStateTy :: C.Type -- Type
topStateTy = [i|struct ${topState}|]

hempty :: HandleCont
hempty = flip ($)

type NodeID  = Int
type Nodes ctx = IntMap (ASTF ctx NodeID)
type IGroup = [NodeID]
type IContext = (Dependencies, Context IGroup)

groupInfo :: IGroup
  -> PeregrineC (C.Identifier, CompInfo)
groupInfo group = case group of
  []   -> do
    let    
      src      = C.Identifier topState
      ref      = C.Exp topState
      compInfo = CompInfo src ref errorty dempty False hempty hempty
      set      = maybe (Just (src, compInfo, [])) Just
    modify (\t -> t { groups = Map.alter set group (groups t) })
    appendTopsort compInfo
    return (src, compInfo)

  g:gs -> do
    st <- groups <$> get
    case Map.lookup group st of
      -- we've already compiled the group
      Just (i, t, _) -> return (i, t)
      Nothing -> do
        (mapName, compInfo) <- genGroupInfo g gs
        appendTopsort compInfo
        return (mapName, compInfo)
           
  where    
    errorty = error "Can't propagate type of group!"
           
    genGroupInfo g gs = do
      parent <- ref . snd <$> groupInfo gs
      -- the type of the struct
      groupId <- liftC $ C.genId "group_map"
      iterId  <- liftC $ C.genId "group"
      g <- compileAST g
           
      let  
        key     = ref g
        kty     = ctype g
        deps    = dependencies g
        iter    = [i|struct ${iterId}|]
        iterPtr = [i|struct ${iterId} *|]
        map_    = withCtx parent groupId
        lhs     = withCtx parent iterId
        h       = \msg next -> if msg `Set.member` (_deps deps)
          then next msg >>= \next -> return $ trim [i|
            if (! ${map_}.count(${key})) {
              ${iter} tmp;/*TODO sane initialization*/
              ${map_}[${key}] = tmp;
            }
            //Set the pointer so future dereferences within this callback
            //will refer to the right thing
            ${lhs} = &((${map_})[${key}]);
            ${next}
            |]
          else next msg
          
        ret = CompInfo
          { src          = iterId
          , ref          = withCtx parent iterId
          , ctype        = errorty
          , dependencies = error "Can't access Group deps"
          , tmp          = False
          , handler      = h
          , cleanup      = hempty
          }
          
      mapTy <- liftC $ CL.cpp_unordered_map kty iter

      let
        iterData = (iterPtr, iterId)
        mapData  = (mapTy, groupId)
        set Nothing = error "Didn't find group in map.."
        set (Just (i, compInfo, elems)) = Just
          (i, compInfo, iterData : mapData : elems)

      -- Insert self into global list of structs
      modify (\t -> t
        { groups = Map.insert group (groupId, ret, []) (groups t) })
      -- Insert self into parent struct
      modify (\t -> t { groups = Map.alter set gs (groups t) })

      return (groupId, ret)

compileConstant :: Constant -> CompInfo
compileConstant c = CompInfo src value ty dempty tmp hempty hempty
  where    
    src         = error "No variable for constant"
    tmp         = True
    (value, ty) = first C.Exp $ case c of
      ConstInt    i -> (show i, C.int)
      ConstDouble d -> (show d, C.double)
      ConstBool   b -> (showB b, C.bool)
      where
        showB b = if b then "true" else "false"
           
compileZipWith :: IContext
  -> BinOp 
  -> CompInfo
  -> CompInfo
  -> PeregrineC CompInfo
compileZipWith (deps, ctx) op x y = do

  myId  <- genLocalId "zip" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
           
  let      
    ann        = maybe "" (\x -> "/*"++x++"*/") (annotation ctx)
    storage    = storageRequirement deps
    tmp        = temporary storage
    out        = if tmp
      then zipExp
      else withCtx group myId
    zipExp     = fromString ann
      <> compileOp op (ref x `C.cast` ty) (ref y `C.cast` ty)
    ty1        = ctype x
    ty2        = ctype y
    ty         = if
      | ty1 == ty2
        -> ty1
      | not (C.isNumeric ty1) || not (C.isNumeric ty2)
        -> error $ "Can't zip types " <> show ty1 <> ", " <> show ty2
      | op == Div || C.double `elem` [ty1, ty2]
        -> C.double
      | op `elem` [Add, Mul, Sub] && (C.signed ty1 || C.signed ty2)
        -> C.signedOf $
          if C.width ty1 >= C.width ty2 then ty1 else ty2
      | op `elem` [Add, Mul, Sub] -- neither is signed
        -> (if C.width ty1 >= C.width ty2 then ty1 else ty2)
      | op `elem` [Gt, Ge, Lt, Le, Eq]
        -> C.bool
      | otherwise
        -> error $ "Can't zip numeric types??" <> show ty1 <> ", " <> show ty2
          
  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps) && (not tmp)
      then next msg >>= \next -> return $ trim [i|
        ${out} = ${zipExp};
        ${next}
        |]
      else next msg

compileMap :: IContext
  -> UnaryOp
  -> CompInfo
  -> PeregrineC CompInfo
compileMap (deps, ctx) op x = do
           
  myId  <- genLocalId "fmap" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
           
  case op of
    Math {} -> liftC . void $ C.include "cmath"
    _       -> return ()
           
  let      
    ty      = C.double -- TODO revisit
    storage = storageRequirement deps
    tmp     = temporary storage
    mapExp  = case op of
      Math f   -> [i|${f}(${ref x})|]
      Cast ty  -> ref x `C.cast` ty
      Window f -> [i|${ref x}.${f}()|]
    out     = if tmp
      then mapExp
      else withCtx group myId

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps) && (not tmp)
      then next msg >>= \next -> return $ trim [i|
        ${out} = ${mapExp};
        ${next}
        |]
      else next msg

compileMerge :: IContext
  -> CompInfo
  -> CompInfo
  -> PeregrineC CompInfo
compileMerge (deps, ctx) x y = do
          
  myId <- genLocalId "merge" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
          
  let     
    out        = withCtx group myId
    ty         = if ctype x == ctype y
      then ctype x
      else error $ "Tried to merge two unequal types, I don't know what to do"
    tmp        = False -- TODO revisit
          
  return $ CompInfo myId out ty deps tmp hempty $ \msg next -> do
    next <- next msg
    -- if it's in both deps then it will default to the left
    return $ if
      | msg `Set.member` (_deps (dependencies x)) -> trim [i|
        ${out} = ${ref x};
        ${next}
        |]
      | msg `Set.member` (_deps (dependencies y)) -> trim [i|
          ${out} = ${ref y};
          ${next}
          |]
      | otherwise -> next

compileProjection :: IContext
  -> Projection
  -> PeregrineC CompInfo
compileProjection (deps, ctx) p = do
  myId  <- genLocalId "projection" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
           
  let      
    storage       = storageRequirement deps
    tmp           = temporary storage
    out           = if tmp
      then cprojection
      else withCtx group myId
    cprojection   = [i|msg.${msgName}.${fieldName}|]
    (field, pmsg) = resolveProjection p
    msgName       = cnm $ _msgName pmsg
    fieldName     = cnm $ _name field
           
  ty <- liftC $ _cty (_atype field)
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg == pmsg && permanent storage
      then next msg >>= \next -> return $ trim [i|
        ${out} = ${cprojection};
        ${next}
        |] 
      else next msg

compileWindow :: IContext
  -> BinOp 
  -> Int   
  -> CompInfo
  -> CompInfo
  -> PeregrineC CompInfo
compileWindow (deps, ctx) op span t x = do
  myId   <- genLocalId "rolling_window" ctx
  group  <- ref . snd <$> groupInfo (nodeGroup ctx)
  elemTy <- liftC $ CL.pair (ctype t) (ctype x)
           
  let      
    k1 x   = [i|${x}.second|]
    k2 x y = compileOp op x y
           
  ty     <- liftC $ CL.rolling_window elemTy (ctype x) k1 k2
  let      
    out = withCtx group myId
    tmp = False
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next -> do
    next <- next msg
    if msg `Set.member` (_deps deps)
      then do
        return $ trim [i|

          ${out}.push(std::make_pair(${ref t}, ${ref x}));
          while (${out}.size() && ${out}.peek_front().first - ${out}.peek_back().first > ${span}) {
            ${out}.pop();
          }
          
          ${next}
          
        |]
      else return next

-- TODO this needs to be reasoned about more.
-- Can something from a guard escape its guard scope? (probably should be able to)
compileGuard :: IContext
  -> CompInfo
  -> CompInfo
  -> PeregrineC CompInfo
compileGuard (deps, ctx) pred ast = do
  myId <- genLocalId "guard" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  pred  <- pure (ref pred) -- TODO add check that pred is type bool
  ref   <- pure (ref ast)
  ty    <- pure (ctype ast)
  let      
    out = withCtx group myId
    tmp = False -- TODO can do better?
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps)
      then next msg >>= \next -> return $ trim [i|
         if (${pred}) {
           ${out} = ${ref};
           ${next}
         } 
       |]  
      else next msg

compileLast :: IContext
  -> CompInfo
  -> PeregrineC CompInfo
compileLast (deps, ctx) src = do

  myId  <- genLocalId "last" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  ref   <- pure (ref src)
  ty    <- pure (ctype src)
  let      
    out = withCtx group myId
    tmp = False
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps)
      then next msg >>= \next -> return $ trim [i|
          // Do whatever depends on the last value
          ${next}
          // Finally, assign to storage
          ${out} = ${ref};
        |] 
      else next msg

compileRestrict :: IContext
  -> CompInfo
  -> CompInfo
  -> PeregrineC CompInfo
compileRestrict (deps, ctx) x _y = do

  myId  <- genLocalId "restrict" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  ref   <- pure (ref x)
  ty    <- pure (ctype x)
           
  let      
    storage = storageRequirement deps
    tmp     = temporary storage
    out     = if tmp
      then ref
      else withCtx group myId
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next -> do
    next <- next msg
    return $ if permanent storage && msg `Set.member` (_deps deps)
      then trim [i|
          ${out} = ${ref}
          ${next}
        |] 
      else next

compileFold :: IContext
  -> BinOp 
  -> CompInfo
  -> PeregrineC CompInfo
compileFold (deps, ctx) op src = do
  myId <- genLocalId "fold" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  ref <- pure (ref src)
  ty  <- pure (ctype src)
  let      
    out = withCtx group myId
    tmp = False
    comment = maybe "" ("// update "++) (annotation ctx)
           
  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    let exp msg = compileOp op out ref
    in if msg `Set.member` (_deps deps)
         then next msg >>= \next -> return $ trim [i|
           ${comment}
           ${out} = ${exp msg};
           ${next}
           |]
         else next msg

compileObserve :: IContext
  -> Observation
  -> CompInfo
  -> PeregrineC CompInfo
compileObserve (deps, ctx) observeType x = do
  group     <- ref . snd <$> groupInfo (nodeGroup ctx)
  nodes     <- nodeContext <$> ask
  proto     <- specification <$> ask
  groupKeys <- mapM compileAST (nodeGroup ctx)
  groups    <- mapM groupInfo (tails (nodeGroup ctx))
           
  let      
    tmp = True
    fmt :: C.Type -> C.Exp -> C (C.Exp, C.Exp)
    fmt ty e = do
      sym <- TAQ.symbol
      return $ if
        | ty == sym       -> ("%.8s", [i|(char *)(&(${e}))|])
        | ty == C.double -> ("%f", e)
        | ty == C.long   -> ("%ld", e)
        | ty == C.ulong  -> ("%lu", e)
        | C.signed ty    -> ("%d", e)
        | otherwise       -> ("%u", e)
    toPrint = map (ctype &&& ref) $ reverse $ x : groupKeys
    commas  = intercalate "," . map C.getExp
    handler = case observeType of
      Every   -> \msg next -> if msg `Set.member` (_deps deps)
          then do
            next <- next msg
            (fmts, exps) <- unzip <$> mapM (uncurry fmt) toPrint
            return $ trim [i|
              printf("${commas fmts}\\n", ${commas exps});
              ${next}
              |]
          else next msg
      Summary -> hempty
    cleanup = case observeType of
      Summary -> \msg next -> runOnce msg (next msg) $ do
        (fmts, exps) <- unzip <$> mapM (uncurry fmt) toPrint
        next <- next msg
        let gs = reverse $ zip (x : groupKeys) groups
        return $ forLoop fmts exps gs ++ next
      Every   -> hempty
      where
        -- only generate code for one message (or once if there are no messages)
        runOnce msg done code = if maybe True (msg ==) (maybeHead msgs)
          then code
          else done
        msgs = _outgoingMessages proto
        forLoop fmts exps gs = go gs
          where
            gid gs = intercalate "->" $ C.getIdentifier . fst . snd <$> gs
            go []     = error "Programmer error: Recursed too far"
            go [x]    = [i|printf("${commas fmts}\\n", ${commas exps});|]
            go (g:gs) = let
              kref = ref (fst g)       -- state->key
              g'   = case maybeHead gs of
                Nothing -> error "Invariant violated: list < len 1"
                Just g  -> g
              gref = ref (snd (snd g')) -- state->group
              in trim [i|for (auto it = ${gid (g:gs)}.begin(); it != ${gid (g:gs)}.end(); ++it)
                {
                  ${kref} = it->first;
                  ${gref} = &(it->second);
                  ${go gs}
                }
              |]
           
  return $ CompInfo (src x) (ref x) (ctype x) deps tmp cleanup handler
           
assignNodeIds ::Signal -> (NodeID, Nodes (Context IGroup))
assignNodeIds ast = (,) root (IMap.fromList . map swap . Map.toList $ st)
  where    
    (root, st) = runState (go ast) mempty
    runGroup (Context (Group g) a) = do
      g <- mapM go g
      return (Context g a)
    go (Fix ast) = do
      -- assign node ids for all the group signals
      ast <- mapM go ast
      ast <- mapCtxM runGroup ast
      st  <- get
      case Map.lookup ast st of
        Just nid -> return nid
        Nothing  -> do
          let nid = Map.size st
          put (Map.insert ast nid st)
          return nid

-- calculate dependencies
calcDeps :: NodeID
  -> Nodes (Context IGroup)
  -> Nodes IContext
calcDeps root nodes = IMap.mapWithKey (\k -> mapCtx ((st !. k),)) nodes
  where   
    m !. k = case IMap.lookup k m of
      Nothing -> dempty
      Just t  -> t
    st = execState (go root) mempty
          
    fwdDeps nid cnid = do
      go cnid
      cdeps <- maybe dempty id . IMap.lookup cnid <$> get
      modify $ IMap.insertWith
        (\new old -> old { _deps = _deps old <> _deps new })
        nid
        (Deps (_deps cdeps) mempty False)
    revDeps nid cnid = do
      deps <- maybe dempty id . IMap.lookup nid <$> get
      modify $ IMap.adjust
        (\c -> c { _revdeps = _revdeps c <> _deps deps }) cnid
    depends nid cnid = do
      nid `fwdDeps` cnid
      nid `revDeps` cnid

    go nid = mapCtxM (mapM go . nodeGroup) ast >> case ast of
      ZipWith _ _ x y -> do
        nid `depends` x
        nid `depends` y
      MergeExp _ x y -> do
        nid `depends` x
        nid `depends` y
      MapExp _ _ x -> do
        nid `depends` x
      FoldExp _ _ x -> do
        nid `depends` x
      ProjectExp _ p -> do
        let (_field, pmsg) = resolveProjection p
        modify $ IMap.insert nid (Deps (Set.singleton pmsg) mempty False)
      WindowExp _ _ p t x -> do
        nid `revDeps` p
        nid `revDeps` t
        -- ^ don't need to update when the timestamp is updated,
        -- but do need to have it available
        nid `depends` x
      GuardExp _ pred x -> do
        nid `depends` x
        nid `revDeps` pred
      LastExp _ x -> do
        nid `depends` x
      RestrictExp _ x y -> do
        go y
        modify $ \st -> IMap.insert nid (st !. y) st
        -- if x is trade price and y is bid price,
        -- we need to store x so that when we reference
        -- it from the quote domain it is available.
        nid `revDeps` x
      ObserveExp _ Every x -> do
        nid `depends` x
      ObserveExp _ Summary x -> do
        nid `depends` x
        modify $ IMap.adjust (\c -> c { _mustStore = True }) x
      ConstExp {} -> return ()
      where
        ast = nodes IMap.! nid

compileAST :: NodeID -> PeregrineC CompInfo
compileAST root = go root -- could this use hyloM?
  where    
    deref nid = do
      nodes <- nodeContext <$> ask
      case IMap.lookup nid nodes of
        Nothing -> error "Nonexistent NodeID!"
        Just t  -> return t
    go nid = (deref nid >>=) $ \ast -> case ast of
      ZipWith ctx op x y  -> compileOnce nid ctx $ do
        x <- go x
        y <- go y
        compileZipWith ctx op x y
      MergeExp ctx x y    -> compileOnce nid ctx $ do
        x <- go x
        y <- go y
        compileMerge ctx x y
      MapExp ctx f x      -> compileOnce nid ctx $ do
        x <- go x
        compileMap ctx f x
      FoldExp ctx op x    -> compileOnce nid ctx $ do
        x <- go x
        compileFold ctx op x
      ProjectExp ctx p    -> compileOnce nid ctx $ do
        compileProjection ctx p
      WindowExp ctx op s t x -> compileOnce nid ctx $ do
        t <- go t
        x <- go x
        compileWindow ctx op s t x
      LastExp ctx x       -> compileOnce nid ctx $ do
        x <- go x
        compileLast ctx x
      GuardExp ctx pred x -> compileOnce nid ctx $ do
        pred <- go pred
        x    <- go x
        compileGuard ctx pred x
      RestrictExp ctx x y -> compileOnce nid ctx $ do
        x <- go x
        y <- go y
        compileRestrict ctx x y
      ObserveExp ctx o x  -> compileOnce nid ctx $ do
        x <- go x
        compileObserve ctx o x
      ConstExp c -> return (compileConstant c)
           
    -- Only compile the node if it hasn't been compiled yet.
    -- It is IMPORTANT to only compile it once otherwise the topsort
    -- won't work.
    compileOnce nid (d, Context group _) action = do
      mexists <- (IMap.lookup nid . nodeInfo) <$> get
      case mexists of
        Just compInfo -> return compInfo
           
        Nothing       -> do
          compInfo <- action
           
          appendTopsort compInfo
           
          -- mark seen
          modify $ \st -> st
            { nodeInfo = IMap.insert nid compInfo (nodeInfo st) }
           
          -- insert its declaration into its container if it needs to be stored
          let
            structData = (ctype compInfo, src compInfo)
            set Nothing = error "Didn't find group info .. "
            set (Just (i, compInfo, elems)) = Just
              (i, compInfo, structData : elems)
           
          when (not (tmp compInfo) || _mustStore d) $ modify $ \st -> st
            { groups = Map.alter set group (groups st) }
           
          return compInfo

runPeregrine :: Peregrine -> Signal
runPeregrine peregrine = runReader peregrine (Group [])

runIntermediate :: CompEnv -> PeregrineC a -> C (a, CompState)
runIntermediate env p = runReaderT
  (runStateT p (CompState mempty mempty mempty))
  env

codegen :: Specification a -> Peregrine -> C C.Func
codegen spec peregrine = do
          
  let     
    ast   = runPeregrine peregrine
    (root, nodeIds) = assignNodeIds ast
    nodes = calcDeps root nodeIds
    env   = CompEnv (cproto spec) nodes
  iast <- runIntermediate env (compileAST root)
          
  let     
    (CompState nodeInfo struct nodes) = snd iast
          
  forM (reverse (Map.elems struct)) $ \(_name, compInfo, elems) -> do
    -- TODO sort elems by C width
    cstruct (src compInfo) elems
          
  foo <- C.genId "foo"
  C.topDecl [i|${topStateTy} ${foo}|]
  C.topDecl [i|${topStateTy} *${topState} = &${foo}|]
  let     
    handle         = runHandleCont (handler `map` toList nodes)
    cleanupHandler = runHandleCont (cleanup `map` toList nodes)
    empty   = const (pure [])
  cmain spec (CP.MsgHandler handle empty cleanupHandler)

