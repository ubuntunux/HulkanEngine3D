# LambdaEngine3D

### fork from vulkan-api
  * [https://github.com/achirkin/vulkan](https://github.com/achirkin/vulkan/)
    
### Test
Tested using `stack` on:

  * Windows 10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Mac OS High Sierra 10.13.4 with [MoltenVK](https://github.com/KhronosGroup/MoltenVK)
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  
Generate haskell vulkan sources using vk.xml file.
To update the api bindings, run `genvulkan` using stack with this project folder:
```bash
git clone https://github.com/ubuntunux/LambdaEngine3D
cd genvulkan
stack build
stack exec genvulkan
cd ..
stack runhaskell Main.hs
```

