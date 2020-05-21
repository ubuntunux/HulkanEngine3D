#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 1) uniform sampler2D textureAlbedo;
layout(binding = 2) uniform sampler2D textureMaterial;
layout(binding = 3) uniform sampler2D textureNormal;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

void main() {
    vec3 albedo = texture(textureAlbedo, texCoord).xyz;
    vec3 material = texture(textureMaterial, texCoord).xyz;
    vec3 normal = texture(textureNormal, texCoord).xyz;
    outColor.xyz = albedo;
    outColor.w = 1.0;
}
