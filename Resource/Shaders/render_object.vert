#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"

layout( push_constant ) uniform PushConstant {
  mat4 model;
} pushConstant;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec4 inColor;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec4 vertexColor;
layout(location = 1) out vec3 vertexNormal;
layout(location = 2) out vec2 texCoord;

void main() {
    gl_Position = viewProjectionConstants.VIEW_PROJECTION * pushConstant.model * vec4(inPosition, 1.0);
    vertexColor = inColor;
    vertexNormal = inNormal;
    texCoord = inTexCoord;
}
