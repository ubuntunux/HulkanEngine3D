#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"

layout(binding = 3) uniform sampler2D textureColor;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

void main() {
    vec4 color = texture(textureColor, texCoord);
//    outColor.xyz = vec3(pow(color.x, 20.0));
//    outColor.w = 1.0;
    outColor = color;
}