{-# LANGUAGE OverloadedStrings #-}

module Hrange.Evaluator (
  runEval,
  eval
) where

import           Hrange.Types

import           Control.Lens           (at, non, (^.), sequenceOf, both)
import           Control.Monad
import           Control.Monad.Extra (anyM, concatMapM)
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer   (runWriterT)
import           Data.Foldable          (toList)
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Text.Printf            (printf)
import qualified Text.Regex.TDFA        as R

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
eval (Intersection a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (R.matchTest rx . T.unpack) lhs

eval (Intersection a b) = S.intersection <$> eval a <*> eval b

eval (Difference a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (not . R.matchTest rx . T.unpack) lhs

eval (Difference a b)   = S.difference <$> eval a <*> eval b

eval FunctionAllClusters = do
  state <- ask
  return . makeResult . M.keys $ state ^. clusters

eval (FunctionClusters names) = eval $ FunctionHas (mkConst "CLUSTER") names

eval (FunctionMem clustersExpr namesExpr) = do
  clustersSet <- eval clustersExpr
  nameSet     <- eval namesExpr

  candidates  <- concatMapM pairWithKeys (toList clustersSet)
  matched     <- filterByMember nameSet candidates

  return . makeResult $ map extractKey matched

  where
    extractKey = snd

    pairWithKeys c = do
      ks <- clusterLookupKey c "KEYS"
      return $ map ((,) c) (toList ks)

eval (FunctionHas keysExpr namesExpr) = do
  state      <- ask
  keySet     <- eval keysExpr
  nameSet    <- eval namesExpr

  candidates <- pure $ sequenceOf both (allClusterNames state, toList keySet)
  matched    <- filterByMember nameSet candidates

  return . makeResult . map extractName $ matched

  where
    extractName = fst

    allClusterNames state = M.keys $ state ^. clusters

eval (Product []) = return mempty
eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map toList results :: [[Identifier]]
  let combined = map T.concat $ sequence asList :: [Identifier]

  return . makeResult $ combined

eval (ClusterLookup namesExpr keysExpr) = do
  nameSet <- eval namesExpr
  keySet  <- eval keysExpr

  foldMap (\name -> foldMap (clusterLookupKey name) keySet) nameSet

eval (NumericRange prefix width low high) = do
  let nums = map (T.pack . printf ("%0" ++ show width ++ "i")) [low..high] :: [Identifier]

  return . makeResult $ map (prefix <>) nums

-- Some implementations return %{allclusters()} matched against the regex. On a
-- suspicion that this a pattern that should be discouraged, I'm opting here to
-- return empty and prevent the parser from generating regex expression outside
-- of an intersection or difference. Those implementations provide special
-- cased actual behaviour for Regexp without recursing here.
--
-- If this turns out to be a good decision, I'll consider encoding it into the
-- type system.
eval (Regexp _) = return mempty

filterByMember :: Foldable t => t Identifier -> [(ClusterName, ClusterKey)] -> Eval [(ClusterName, ClusterKey)]
filterByMember ns = filterM (\(c, k) -> anyM (inKey c k) (toList ns))

clusterLookupKey :: ClusterName -> ClusterKey -> Eval Result
clusterLookupKey name "KEYS" = do
    state <- ask

    return . makeResult . M.keys $ state
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
inKey c k n = do
                ns <- clusterLookupKey c k
                return $ n `S.member` ns
