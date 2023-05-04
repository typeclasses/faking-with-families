module Main (main) where

import Relude

import HighLevel
import Fake

import Test.Hspec (hspec, shouldBe, specify)

data Q a = Q { q1 :: a, q2 :: a, q3 :: a }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

qSpec :: Q (QueueSpecification Fake)
qSpec = Q
  { q1 = QueueSpecification "transfer" 1
  , q2 = QueueSpecification "graphics" (1/3)
  , q3 = QueueSpecification "graphics" (2/3)
  }

main :: IO ()
main = hspec do
  specify "createDevice" do
    Device _ qs <- createDevice @Fake () qSpec
    qs `shouldBe` qSpec
