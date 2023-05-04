module Fake where

import Relude

import Data.Vector qualified as Vector
import Data.Map.Strict qualified as Map

import LowLevel
import HighLevel

data Fake

instance (MonadFail m) => Vulkan Fake m where

    type VkPhysicalDevice Fake = ()
    type VkFamily Fake = Text
    type VkPriority Fake = Rational
    type VkQueue Fake = QueueSpecification Fake
    type VkDevice Fake = Map (VkQueueId Fake) (QueueSpecification Fake)

    vkCreateDevice () = pure . Map.fromList . concatMap f . Vector.toList
      where
        f (VkQueueFamilySpecification family priorities) =
            zip [0 ..] (Vector.toList priorities) <&> \(i, p) ->
                (VkQueueId family i, QueueSpecification family p)

    vkGetDeviceQueue m i = Map.lookup i m & maybe (fail "") pure
