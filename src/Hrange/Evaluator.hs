{-# LANGUAGE OverloadedStrings #-}

module Hrange.Evaluator (
  runEval,
  eval
) where

import           Hrange.Text
import           Hrange.Types

import           Control.Lens           (at, both, non, sequenceOf, view, (^.))
import           Control.Monad
import           Control.Monad.Extra    (anyM, concatMapM)
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer   (runWriterT)
import           Data.Foldable          (toList, fold)
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))

import           Prelude                hiding (concat)

-- Reducing duplication doesn't make sense for this suggestion
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

runEval :: State -> Eval a -> (a, [RangeLog])
runEval state e = let (x, y) = runIdentity (runReaderT (runWriterT e) state) in
                   (x, toList y)

eval :: Expression -> Eval Result

eval (Const name) = return $! S.singleton name

eval (Union a b) = mappend <$> eval a <*> eval b

eval (Intersection (Regexp _) (Regexp _)) = return mempty
eval (Intersection (Regexp lhs) rhs) = eval (Intersection rhs (Regexp lhs))
eval (Intersection a (Regexp rx)) = S.filter (match rx) <$> eval a
eval (Intersection a b) = S.intersection <$> eval a <*> eval b

eval (Difference a (Regexp rx)) = S.filter (not . match rx) <$> eval a
eval (Difference a b) = S.difference <$> eval a <*> eval b

eval FunctionAllClusters = makeResult . M.keys . view clusters <$> ask

eval (FunctionMem clustersExpr namesExpr) = do
  cs <- evalAsList clustersExpr
  ns <- evalAsList namesExpr

  candidates  <- concatMapM pairWithKeys cs
  matched     <- filterByMember ns candidates

  makeResultM $ map extractKey matched

  where
    extractKey = snd
    pairWithKeys c = map ((,) c) . toList <$> clusterLookupKey c "KEYS"

eval (FunctionHas keysExpr namesExpr) = do
  state <- ask
  ks    <- evalAsList keysExpr
  ns    <- evalAsList namesExpr

  candidates <- pure $ sequenceOf both (allClusterNames state, ks)
  matched    <- filterByMember ns candidates

  makeResultM . map extractName $ matched

  where
    allClusterNames state = M.keys $ state ^. clusters
    extractName = fst

eval (Product []) = return mempty
eval (Product xs) = do
  results <- mapM evalAsList xs

  let combined = map concat . sequence $ results

  makeResultM combined

eval (ClusterLookup namesExpr keysExpr) = do
  ns <- evalAsList namesExpr
  ks <- evalAsList keysExpr

  fold [clusterLookupKey n k | n <- ns, k <- ks]

eval (NumericRange prefix width low high) =
   makeResultM . map ((prefix <>) . leftpad width) $ [low..high]

-- Some implementations return %{allclusters()} matched against the regex. On a
-- suspicion that this a pattern that should be discouraged, I'm opting here to
-- return empty and prevent the parser from generating regex expression outside
-- of an intersection or difference. Those implementations provide special
-- cased actual behaviour for Regexp without recursing here.
--
-- If this turns out to be a good decision, I'll consider encoding it into the
-- type system.
eval (Regexp _) = return mempty

evalAsList :: Expression -> Eval [Identifier]
evalAsList = fmap toList . eval

filterByMember :: Foldable t => t Identifier -> [ClusterPair] -> Eval [ClusterPair]
filterByMember ns = filterM (\(c, k) -> anyM (inKey c k) (toList ns))

clusterLookupKey :: ClusterName -> ClusterKey -> Eval Result
clusterLookupKey name "KEYS" = do
  state <- ask

  makeResultM . M.keys $ state
    ^. clusters
    ^. at name . non mempty

clusterLookupKey name key = do
  state <- ask

  let pretty = "%" <> name <> ":" <> key
  let cache = state ^. clusterCache . non mempty
  let cacheMiss = do
                    recordStat $ CacheMiss pretty
                    foldMap eval $ state
                      ^. clusters
                      ^. at name . non mempty
                      ^. at key . non []
  let cacheHit = \c -> do
                         recordStat $ CacheHit pretty
                         return $ c ^. at key . non mempty

  maybe cacheMiss cacheHit (cache ^. at name)

inKey :: ClusterName -> ClusterKey -> Identifier -> Eval Bool
inKey c k n = S.member n <$> clusterLookupKey c k

makeResultM :: (Monad m) => [Identifier] -> m Result
makeResultM = return . makeResult
