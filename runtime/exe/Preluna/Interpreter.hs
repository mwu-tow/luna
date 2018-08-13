{-# LANGUAGE OverloadedStrings #-}

module Preluna.Interpreter where

import Prologue

import OCI.Data.Name (Name, Qualified(Qualified))
import qualified Control.Monad.State.Layered           as State
import qualified Data.Map                              as Map
import qualified Luna.Runtime as Runtime
import qualified GHC.Exts           as GHC
import Data.Map            (Map)




newtype LocalScope = LocalScope
    { _localVars :: Map Int Runtime.Data }
    deriving Default
makeLenses ''LocalScope

localLookup :: Int -> LocalScope -> Maybe Runtime.Data
localLookup e = Map.lookup e . view localVars

localInsert :: Int -> Runtime.Data -> LocalScope -> LocalScope
localInsert e d = localVars %~ Map.insert e d

merge :: Map Int Runtime.Data -> LocalScope -> LocalScope
merge m = localVars %~ Map.union m




instance Read Name where
    readsPrec = fmap (_1 %~ convert) .: readsPrec @String

deriving instance Read Qualified

data AST = RawString Text
         | Number    Integer
         | Var       Int
         | Def       Qualified Name
         | Lam       AST AST
         | App       AST AST
         | Acc       AST Name
         | Unify     AST AST
         | Seq       AST AST
         | Cons      Qualified Name Name [AST]
         | Match     AST [AST]
         deriving (Show, Read)

instance GHC.IsList AST where
    type Item AST = AST

    fromList [a] = a
    fromList (a:as) = Seq a (GHC.fromList as)

    toList (Seq a b) = a : GHC.toList b
    toList t = [t]

instance IsString AST where
    fromString = RawString . convert

interpret' :: MonadIO m => Runtime.Units -> AST -> m Runtime.Value
interpret' glob t = do
    x <- interpret glob t
    pure $ State.evalT x def

interpret :: MonadIO m => Runtime.Units -> AST -> m (State.StateT LocalScope Runtime.Eff Runtime.Data)
interpret glob = \case
    RawString t -> pure $ pure $ Runtime.toData glob t
    Number    i -> pure $ pure $ Runtime.toData glob i
    Var       v -> pure $ do
        val <- State.gets @LocalScope (localLookup v)
        case val of
            Just v  -> pure v
            Nothing -> lift $ Runtime.throw $ convert $ "Var not found: " <> show v
    Def m n -> lift . Runtime.force <$> Runtime.lookupSymbol glob m n
    Lam i o -> do
        bodyVal <- interpret glob o
        inputMatcher <- irrefutableMatcher i
        pure $ do
            env <- State.get @LocalScope
            return $ Runtime.Function $ \d -> do
                newBinds <- inputMatcher $ Runtime.mkThunk d
                State.evalT bodyVal $ merge newBinds env
    App f a -> do
        fun <- interpret glob f
        arg <- interpret glob a
        pure $ do
            env <- State.get @LocalScope
            let fun' = State.evalT fun env
                arg' = State.evalT arg env
            lift $ Runtime.force $ Runtime.applyFun fun' arg'
    Acc a' name -> do
        a <- interpret glob a'
        pure $ do
            arg <- a
            lift $ Runtime.force $ Runtime.dispatchMethod name arg
    Unify (Var l) r' -> do
        rhs <- interpret glob r'
        pat <- irrefutableMatcher (Var l)
        pure $ do
            env <- State.get @LocalScope
            let rhs' = State.evalT rhs (localInsert l rhsT env)
                rhsT = Runtime.mkThunk rhs'
            rhsV <- lift $ Runtime.runError $ Runtime.force rhs'
            case rhsV of
                Left e -> do
                    State.modify_ @LocalScope $ localInsert l
                        $ Runtime.Error $ unwrap e
                    lift $ Runtime.throw $ unwrap e
                Right v -> do
                    State.modify_ @LocalScope . merge =<< lift (pat v)
                    return v
    Seq l' r' -> do
        lhs <- interpret glob l'
        rhs <- interpret glob r'
        pure $ do
            lhsV <- lhs
            lift $ Runtime.forceThunks' lhsV
            rhs
    Cons mod cls name fs -> do
        fields <- traverse (interpret glob) fs
        let mets = Runtime.getObjectMethodMap glob mod cls
        pure $ do
            fs <- sequence fields
            let cons = Runtime.Constructor name fs
            return $ Runtime.Cons $ Runtime.Object mod cls cons mets
    Match t' cls' -> do
        target  <- interpret glob t'
        clauses <- for cls' $ \case
            Lam pat res -> (,) <$> matcher pat <*> interpret glob res
        pure $ do
            env <- State.get @LocalScope
            let tgt' = State.evalT target env
            tgt <- lift $ Runtime.force tgt'
            (scope, cl) <- lift $ runMatch clauses tgt
            lift $ State.evalT cl (merge scope env)



type MatchRes  = (Maybe (Map Int Runtime.Data), Runtime.Data)
type MatchResM = Runtime.Eff MatchRes
type Matcher   = Runtime.Data -> MatchResM

runMatch :: [(Matcher, a)]
         -> Runtime.Data
         -> Runtime.Eff (Map Int Runtime.Data, a)
runMatch [] _ = Runtime.throw "Inexhaustive pattern match."
runMatch ((m, p) : ms) d = do
    (res, nextObj) <- m d
    case res of
        Just newScope -> return (newScope, p)
        Nothing       -> runMatch ms nextObj

tryConsMatch :: Name -> [Matcher] -> Matcher
tryConsMatch name fieldMatchers d' = Runtime.force' d' >>= \case
    d@(Runtime.Cons (Runtime.Object mod cls (Runtime.Constructor n fs) ms)) ->
        if n == name then matchFields else return (Nothing, d) where
            matchFields :: MatchResM
            matchFields = do
                results <- zipWithM ($) fieldMatchers fs
                let binds = fmap Map.unions $ sequence $ fst <$> results
                    newObj = Runtime.Cons
                               (Runtime.Object mod cls
                                  (Runtime.Constructor n (snd <$> results)) ms)
                return (binds, newObj)
    d -> return (Nothing, d)

tryBoxedMatch :: (Eq a, Runtime.IsNative a) => a -> Matcher
tryBoxedMatch i d' = Runtime.force' d' >>= \case
    d@(Runtime.Native o) -> return (matchRes, d) where
        matchRes = if Runtime.fromNative o == i then Just def else Nothing
    d -> return (Nothing, d)

matcher :: MonadIO m => AST -> m Matcher
matcher = \case
    Var l -> return $ \d -> return (Just $ Map.fromList [(l, d)], d)
    Cons mod cls n as -> do
        argMatchers <- traverse matcher as
        return $ tryConsMatch n argMatchers
    Number n -> return $ tryBoxedMatch n
    RawString s -> return $ tryBoxedMatch s
    s -> return $ const $
        Runtime.throw $ convert $ "Unexpected pattern: " <> show s

irrefutableMatcher :: MonadIO m => AST
                   -> m (Runtime.Data
                          -> Runtime.Eff (Map Int Runtime.Data))
irrefutableMatcher expr = do
    m <- matcher expr
    return $ \d -> do
        res <- fst <$> m d
        maybe (Runtime.throw "Irrefutable pattern match failed.") return res
