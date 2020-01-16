# Hulkan Engine 3D ( Haskell + Vulkan )
  * Hulkan is an open source Haskell 3D engine using the vulkan api.  
    
### Requierments
  * Windows 10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Mac OS High Sierra 10.13.4 with [MoltenVK](https://github.com/KhronosGroup/MoltenVK)
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Haskell bindings for vulkan api
    * [https://github.com/achirkin/vulkan](https://github.com/achirkin/vulkan/)
  
### Installation
```bash
git clone https://github.com/ubuntunux/HulkanEngine3D --recursive
cd HulkanEngine3D/vulkan/genvulkan
stack build
stack exec genvulkan
cd ../..
stack build
stack exec main
```

### Note
    * Triangle (1024 x 768) : 1500fps / 0.66ms
