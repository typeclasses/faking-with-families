module HighLevel (Device (..), QueueSpecification (..), createDevice) where

import Relude

import Data.Vector (Vector)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq

import LowLevel

data Device v q = Device (VkDevice v) (q (VkQueue v))

data QueueSpecification v = QueueSpecification (VkFamily v) (VkPriority v)
deriving stock instance (Eq (VkFamily v), Eq (VkPriority v)) => Eq (QueueSpecification v)
deriving stock instance (Show (VkFamily v), Show (VkPriority v)) => Show (QueueSpecification v)

createDevice :: forall v m q. (Vulkan v m, Ord (VkFamily v), Traversable q) =>
    VkPhysicalDevice v -> q (QueueSpecification v) -> m (Device v q)
createDevice gpu queueSpecifications = do
    let (ids, specs) = makeVkQueueSpecifications @v queueSpecifications
    vkDevice <- vkCreateDevice @v gpu specs
    vkQueues <- traverse (vkGetDeviceQueue @v vkDevice) ids
    pure (Device vkDevice vkQueues)

newtype Builder v = Builder (Map (VkFamily v) (Seq (VkPriority v)))

makeVkQueueSpecifications :: forall v q. (Ord (VkFamily v), Traversable q) =>
    q (QueueSpecification v) -> (q (VkQueueId v), Vector (VkQueueFamilySpecification v))
makeVkQueueSpecifications specs = flip evalState (Builder Map.empty) do
    queueIdentifiers <- traverse add specs
    vkSpecs <- get <&> build
    pure (queueIdentifiers, vkSpecs)

add :: forall v. Ord (VkFamily v) => QueueSpecification v -> State (Builder v) (VkQueueId v)
add (QueueSpecification family priority) = do
    Builder m <- get
    let xs = Map.lookup family m & fromMaybe Seq.empty
    let i = fromIntegral $ Seq.length xs
    put $ Builder $ Map.insert family (xs Seq.:|> priority) m
    pure $ VkQueueId family i

build :: Builder v -> Vector (VkQueueFamilySpecification v)
build (Builder m) = m & Map.toAscList & fmap f & fromList
  where
    f (family, priorities) = VkQueueFamilySpecification family (fromList (toList priorities))
