#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(binding = 3) uniform sampler2D textureAlbedo;
layout(binding = 4) uniform sampler2D textureMaterial;
layout(binding = 5) uniform sampler2D textureNormal;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outMaterial;
layout(location = 2) out vec4 outNormal;
layout(location = 3) out vec2 outVelocity;

void main() {
    outAlbedo = texture(textureAlbedo, vs_output.texCoord) * vs_output.color;
    outMaterial = texture(textureMaterial, vs_output.texCoord);
    outNormal = texture(textureNormal, vs_output.texCoord);
    outNormal.xyz = normalize(vs_output.normal.xyz);
    outVelocity = vec2(0.0);
}
