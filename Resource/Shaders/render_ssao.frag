#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 3) uniform sampler2D textureSceneNormal;
layout(binding = 4) uniform sampler2D textureSceneDepth;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out float outColor;

void main() {
    outColor = texture(textureSceneDepth, texCoord).x;
}
