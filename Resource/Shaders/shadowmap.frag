#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"
#include "utility.glsl"
#include "blending.glsl"

layout(binding = 3) uniform sampler2D textureBase;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out float outColor;

void main() {
    vec4 base_color = texture(textureBase, texCoord);
    outColor = gl_FragCoord.z;
}
