#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightConstants
{
    LIGHT_CONSTANTS light_constants;
};
layout(binding = 3) uniform sampler2D textureAlbedo;
layout(binding = 4) uniform sampler2D textureMaterial;
layout(binding = 5) uniform sampler2D textureNormal;
layout(binding = 6) uniform sampler2D textureSSAO;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

void main() {
    vec3 albedo = texture(textureAlbedo, texCoord).xyz;
    vec3 material = texture(textureMaterial, texCoord).xyz;
    vec3 normal = texture(textureNormal, texCoord).xyz;
    float lighting = saturate(dot(normal, light_constants.LIGHT_DIRECTION) * 0.5 + 0.5);
    float ssao = texture(textureSSAO, texCoord).x;
    outColor.xyz = albedo * lighting * ssao;
    outColor.w = 1.0;
}
