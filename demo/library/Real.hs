module Real where

import Relude hiding (Real)

import Data.Vector (Vector)
import Data.Word (Word32)

import Vulkan qualified
import Vulkan.Zero (zero)
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))
import Vulkan qualified as DeviceQueueCreateInfo (DeviceQueueCreateInfo (..))

import LowLevel

data Real

instance (MonadIO m) => Vulkan Real m where

    type VkFamily Real = Word32
    type VkPriority Real = Float
    type VkQueue Real = Vulkan.Queue
    type VkPhysicalDevice Real = Vulkan.PhysicalDevice
    type VkDevice Real = Vulkan.Device

    vkCreateDevice gpu qSpecs =
        Vulkan.createDevice gpu zero
          { Vulkan.queueCreateInfos = makeQueueCreateInfo <$> qSpecs
          } Nothing
      where
        makeQueueCreateInfo (VkQueueFamilySpecification fam ps) =
          SomeStruct zero
            { DeviceQueueCreateInfo.queueFamilyIndex = fam
            , DeviceQueueCreateInfo.queuePriorities = ps
            }

    vkGetDeviceQueue device (VkQueueId fam i) =
      Vulkan.getDeviceQueue device fam i
