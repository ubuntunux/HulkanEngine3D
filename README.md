# Hulkan Engine 3D ( Haskell + Vulkan )
  * Hulkan is an open source haskell 3D engine using the vulkan api.  
  * Based on haskell bindings for vulkan api
    * [https://github.com/achirkin/vulkan](https://github.com/achirkin/vulkan/)
    
### Requierments
  * Windows 10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Mac OS High Sierra 10.13.4 with [MoltenVK](https://github.com/KhronosGroup/MoltenVK)
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Stack
    * Windows <= 1.9.3 ( higher version has some errors )
    * Linux <= 2.3.1
  
### Installation
```bash
git clone https://github.com/ubuntunux/HulkanEngine3D
stack build
stack exec main
```
