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
    outColor = texture(textureAlbedo, texCoord);
    //outColor = texture(textureMaterial, texCoord);
    //outColor = texture(textureNormal, texCoord);
    outColor.w = 1.0;
}
