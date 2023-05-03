module LowLevel where

import Relude

import Data.Vector (Vector)
import Data.Word (Word32)

class (Monad m) => Vulkan v m where

    type VkFamily v :: Type
    type VkPriority v :: Type
    type VkQueue v :: Type
    type VkPhysicalDevice v :: Type
    type VkDevice v :: Type

    vkCreateDevice :: VkPhysicalDevice v -> Vector (VkQueueFamilySpecification v) -> m (VkDevice v)

    vkGetDeviceQueue :: VkDevice v -> VkQueueId v -> m (VkQueue v)

data VkQueueFamilySpecification v =
    VkQueueFamilySpecification (VkFamily v) (Vector (VkPriority v))

data VkQueueId v = VkQueueId (VkFamily v) Word32
deriving stock instance (Eq (VkFamily v)) => Eq (VkQueueId v)
deriving stock instance (Ord (VkFamily v)) => Ord (VkQueueId v)
