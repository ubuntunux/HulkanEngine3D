#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"

layout(binding = 3) uniform sampler2D textureAlbedo;
layout(binding = 4) uniform sampler2D textureMaterial;
layout(binding = 5) uniform sampler2D textureNormal;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outMaterial;
layout(location = 2) out vec4 outNormal;
layout(location = 3) out vec2 outVelocity;

void main() {
    outAlbedo = texture(textureAlbedo, texCoord) * vertexColor;
    outMaterial = texture(textureMaterial, texCoord);
    outNormal = texture(textureNormal, texCoord);
    outNormal.xyz = normalize(vertexNormal.xyz);
    outVelocity = vec2(0.0);
}
