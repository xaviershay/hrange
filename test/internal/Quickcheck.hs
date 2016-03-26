import           Hrange
import           Hrange.Evaluator
import           Hrange.Types

import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup ""
  [ testGroup "Quickchecks" quickchecks
  ]

quickchecks :: [TestTree]
quickchecks =
  [ testProperty "eval is fully defined" $
      \expr -> runEval emptyState (eval expr) `seq` True
  ]

instance Arbitrary Expression where
  arbitrary = frequency
    [ (1, Const <$> printable)
    , (1, oneof
            [ Intersection  <$> arbitrary <*> arbitrary
            , Union         <$> arbitrary <*> arbitrary
            , Difference    <$> arbitrary <*> arbitrary
            , ClusterLookup <$> arbitrary <*> arbitrary
            , FunctionHas   <$> arbitrary <*> arbitrary
            , FunctionMem   <$> arbitrary <*> arbitrary
            , FunctionClusters <$> arbitrary
            , fromJust . makeShowableRegex <$> scale ((`mod` 10) . abs) (listOf1 $ elements ['a'..'z'])
            , pure FunctionAllClusters
            , Product <$> scale ((`mod` 10) . abs) arbitrary
            , NumericRange <$> printable <*> elements [0..10] <*> smallInt <*> smallInt
            ])
    ]

smallInt :: Gen Integer
smallInt = elements [0..10]

printable :: Gen T.Text
printable = T.pack <$> listOf1 (elements ['a'..'z'])
