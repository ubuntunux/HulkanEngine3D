#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"

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
    float lighting = saturate(dot(normal, lightConstants.LIGHT_DIRECTION) * 0.5 + 0.5);
    float ssao = texture(textureSSAO, texCoord).x;
    outColor.xyz = mix(albedo * lighting, vec3(ssao), vec3(isnan(ssao) ? 1.0 : 0.0));
    outColor.w = 1.0;
}
