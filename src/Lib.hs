{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( module Lib
    ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           hiding (Const)
import           Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           GHC.Generics
import           Text.Printf            (printf)
import           Text.Regex.TDFA        as R

-- TODO: Don't export Identifier, provide constructors.
newtype Identifier a = Identifier T.Text deriving (Show, Eq, Generic)
instance Hashable (Identifier a)

data PreEval
data PostEval

toConst (Identifier k) = Const (Identifier k)

-- Regex doesn't implement Show, Eq, etc which is pretty annoying
data ShowableRegex = ShowableRegex String R.Regex

instance Hashable ShowableRegex where
  hashWithSalt salt (ShowableRegex x _) = hashWithSalt salt x

instance Show ShowableRegex where
  show (ShowableRegex x _) = "/" ++ x ++ "/"

instance Eq ShowableRegex where
  (ShowableRegex x _) == (ShowableRegex y _) = x == y

makeShowableRegex x = do
  rx <- makeRegexM x

  return . Regexp $ ShowableRegex x (rx :: R.Regex)

data Expression =
  Intersection Expression Expression |
  Difference Expression Expression |
  Union Expression Expression |
  ClusterLookup Expression Expression |
  Regexp ShowableRegex |
  FunctionHas Expression Expression |
  FunctionClusters Expression |
  FunctionAllClusters |
  Product [Expression] |
  NumericRange (Identifier PreEval) Int Integer Integer |
  Const (Identifier PreEval)

  deriving (Eq, Show, Generic)


instance Hashable Expression

-- Cluster expressions should be unique (i.e. a set), but that doesn't really
-- buy us anything implementation wise. It's easier (and strictly more accurate
-- to the source data) to store as a list.
type Cluster = M.HashMap (Identifier PostEval) [Expression]

-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet (Identifier PostEval)
type ClusterMap = M.HashMap (Identifier PostEval) Cluster -- TODO: Is PostEval right here?
data State = State { _clusters :: ClusterMap } deriving (Show)
makeLenses ''State

type Eval a = ReaderT State Identity a

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

mapFilterM :: (Monad m) => (Cluster -> m Bool) -> ClusterMap -> m ClusterMap
mapFilterM p clusters = do
  matching <- filterM (p . snd) (M.toList clusters)
  return $ M.fromList matching

mkConst x = Const $ Identifier . T.pack $ x

eval :: Expression -> Eval Result
eval (Const id)         = return $ S.singleton (toResult id)
  where
    toResult (Identifier x) = Identifier x

eval (Union a b)        = S.union        <$> eval a <*> eval b

-- TODO: Fail parse if two regexps
eval (Intersection (Regexp lhs) (Regexp rhs)) = return S.empty
eval (Intersection (Regexp lhs) rhs) = eval (Intersection rhs (Regexp lhs))
eval (Intersection a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (R.matchTest rx . T.unpack . unwrap) lhs
  where
    unwrap (Identifier x) = x

eval (Intersection a b) = S.intersection <$> eval a <*> eval b

-- TODO: Fail parse if diff on LHS
eval (Difference a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (not . R.matchTest rx . T.unpack . unwrap) lhs
  where
    unwrap (Identifier x) = x

eval (Difference a b)   = S.difference   <$> eval a <*> eval b

-- TODO: Type checking for number of args
eval FunctionAllClusters = do
  state <- ask
  return . S.fromList . M.keys $ state ^. clusters

-- TODO: Type checking for number of args
eval (FunctionClusters names) = eval $ FunctionHas (Const (Identifier "CLUSTER")) names

-- TODO: Type checking for number of args
eval (FunctionHas keys names) = do
  state <- ask
  nameSet <- eval names
  keySet  <- eval keys

  matching <- mapFilterM (hasNamesInKeys nameSet keySet) (state ^. clusters)

  return . S.fromList . M.keys $ matching

  where
    hasNamesInKeys names keys cluster = do
      hasAny <- mapM (hasNameInKeys cluster keys) $ S.toList names
      return $ or hasAny

    hasNameInKeys cluster keys name = do
      hasName <- mapM (hasNameInKey cluster name) $ S.toList keys
      return $ or hasName

    hasNameInKey cluster name key = do
      names <- namesAtKey cluster key
      return $ S.member name names

    namesAtKey cluster key = do
      names <- mapM eval (exprsAtKey cluster key)
      return $ foldr S.union S.empty names

    exprsAtKey :: Cluster -> Identifier PostEval -> [Expression]
    exprsAtKey cluster key = cluster ^. at key . non []

eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map S.toList results :: [[Identifier PostEval]]
  let combined = map T.concat $ sequence (map (map unwrap) asList) :: [T.Text]
  let typed    = map toResult combined

  return . S.fromList $ typed

  where
    toResult :: T.Text -> Identifier PostEval
    toResult x = Identifier x

    unwrap (Identifier x) = x


eval (ClusterLookup names keys) = do
  state   <- ask
  nameSet <- eval names
  keySet  <- eval keys

  results <- mapM eval $
    foldMap (\name ->
      foldMap (clusterLookupKey state name) keySet) nameSet

  -- TODO: folding set union maybe not particularly efficient here?
  return $ foldr S.union S.empty results

eval (NumericRange (Identifier prefix) width low high) = do
  let nums = map (T.pack . printf ("%0" ++ show width ++ "i")) [low..high] :: [T.Text]

  return . S.fromList $ map (toResult . (prefix <>)) nums

  where
    toResult :: T.Text -> Identifier PostEval
    toResult x = Identifier x

-- Some implementations return %{allclusters()} matched against the regex. On a
-- suspicion that this a pattern that should be discouraged, I'm opting here to
-- return empty and prevent the parser from generating regex expression outside
-- of an intersection or difference. Those implementations provide special
-- cased actual behaviour for Regexp without recursing here.
--
-- If this turns out to be a good decision, I'll consider encoding it into the
-- type system.
eval (Regexp _) = return S.empty

clusterLookupKey :: State -> Identifier PostEval -> Identifier PostEval -> [Expression]
clusterLookupKey state name (Identifier "KEYS") =
  map toConst $ M.keys $ state
    ^. clusters
    ^. at name . non M.empty

clusterLookupKey state name key =
  state
    ^. clusters
    ^. at name . non M.empty
    ^. at key . non []

emptyState = State { _clusters = M.empty }

addCluster :: T.Text -> Cluster -> State -> State
addCluster name cluster = clusters %~ M.insert (Identifier name) cluster

mkKey :: T.Text -> [Expression] -> Cluster
mkKey name = M.singleton (Identifier name)

fromMap x = State { _clusters = x }
