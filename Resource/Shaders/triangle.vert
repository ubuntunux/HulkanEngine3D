#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "SceneConstants.glsl"

layout( push_constant ) uniform PushConstant {
  mat4 model;
} pushConstant;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec3 fragNormal;
layout(location = 2) out vec2 fragTexCoord;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    gl_Position = sceneConstants.PROJECTION * sceneConstants.VIEW * pushConstant.model * vec4(inPosition, 1.0);
    fragColor = inColor;
    fragNormal = inNormal;
    fragTexCoord = inTexCoord;
}
