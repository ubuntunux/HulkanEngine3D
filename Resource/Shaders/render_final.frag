#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"

layout(binding = 0) uniform sampler2D textureColor;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

void main() {
    // Tonemapping
    vec4 color = texture(textureColor, texCoord);
    color.xyz = Uncharted2Tonemap(color.xyz, 1.0);
    color.xyz *= vignetting(texCoord, 1.0, 0.20);
    color.xyz = Contrast(color.xyz, 1.1);
    outColor = color;
}
