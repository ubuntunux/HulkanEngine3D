# Hulkan Engine 3D ( Haskell + Vulkan )
  * Hulkan is an open source haskell 3D engine using the vulkan api.  
  * Based on haskell bindings for vulkan api
    * [https://github.com/achirkin/vulkan](https://github.com/achirkin/vulkan/)
  * Similar projects
    * [https://github.com/ubuntunux/PyEngine3D](https://github.com/ubuntunux/PyEngine3D)
    
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

### Features
* Screenshot
![HulkanEngine3D](https://github.com/ubuntunux/HulkanEngine3D/blob/master/images/screenshot.png)
* Editor
    - ThreepennyElectron
* Import
    - Mesh 
        - [x] .obj, .dae ( colada )
        - [ ] gltf, blender
    - Texture
        - [x] .png, .tga, .bmp etc 
        - [ ] Compressed Texture (ETC, DDS)
* Light
    - [x] Directional light & Shadow mapping    
    - [ ] Spot light
    - [ ] Area light
    - [ ] Point light
        - [ ] shadow map using dual paraboloid mapping
* Particle System
    - [ ] CPU Based Particle
    - [ ] GPU Based Particle
    - [ ] Vector Field
    - [ ] Particle spawn on polygon surface
    - [ ] Bitonic Sorting
    - [ ] Memory Pool
    - [ ] Attractor
    - [ ] Noise
    - [ ] Curl Noise
* Object
    - [ ] Select, Move, Modify
    - [ ] Gizmo
    - [ ] Skeleton Mesh
    - [x] Static Mesh        
    - [ ] Tree, Foliage, Grass
    - [ ] Terrain
    - [ ] Atmoshpere & Sky
    - [ ] Road
    - [ ] Wind
    - [ ] FFT Ocean
    - [ ] River 
* Rendering
    - [ ] Culling
        - [ ] occlusion culling
        - [ ] distance culling
        - [ ] view frustum culling
    - [ ] VTF Skinning
    - [ ] Calculate the animation in gpu
    - [ ] Distance Field Font 
    - [ ] Real time light probe 
    - [ ] PBR
    - [ ] Temporal AA
    - [ ] SSAA
    - [ ] MSAA
    - [ ] Temporal Upsacle
    - [ ] Screen Space Relfection
    - [ ] Screen Space Ambient Occlusion
    - [ ] Screen Space Bevel
    - [ ] Screen Space SSS    
    - [ ] Depth Of Field
        - [ ] Bokeh
    - [ ] Bloom
    - [ ] Tone mapping
    - [ ] Glare
    - [ ] Film Grain
    - [ ] Color Correction
    - [ ] Color Grading
    - [ ] Light Shaft
    - [ ] Motion Blur
        - [ ] Recursive Velocity
    - [ ] Parallax Occlusion Rendering
    - [ ] Paraboloid environment map    
    - [ ] Voxel Based GI
    - [ ] Volumtric Scattering
    - [ ] Fur Rendering    
* Resource Manager
    - [ ] Load / Save / Import / Export
    - [ ] Unload / Reload system
    - [ ] Duplicate resource
    - [ ] Sound Manager
    - [ ] Script Manager
* Blender3D plugin
    - [ ] live sync geometry, animation, scene datas
    - [ ] edit animation, scene, sequence, particles in blender
* InGame GUI
    - [ ] input / output
    - [ ] progress bar
    - [ ] button
* Optimize
    - [ ] Only dynamic shadows are updated on every frame, and static shadows are not updated every time.
    - [ ] SSR ray reuse in compute shader
    - [ ] Postprocessing in compute shader
    - [ ] FFT in compute shader
    - [ ] Precomputed atmosphere in compute shader 

## References
- https://github.com/achirkin/vulkan
- https://github.com/achirkin/easytensor
- https://github.com/codetalkio/Haskell-Electron-app
- https://github.com/thma/ThreepennyElectron
