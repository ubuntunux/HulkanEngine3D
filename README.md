# LambdaEngine3D

# fork from vulkan-api
    - https://github.com/achirkin/vulkan
    
Tested using `stack` on:

  * Windows 10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Mac OS High Sierra 10.13.4 with [MoltenVK](https://github.com/KhronosGroup/MoltenVK)
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
    
# genvulkan

Generate haskell vulkan sources using vk.xml file.
To update the api bindings, run `genvulkan` using stack with this project folder:
```bash
cd genvulkan
stack build
stack exec genvulkan
```

