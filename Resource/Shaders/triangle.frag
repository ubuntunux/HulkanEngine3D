#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "SceneConstants.glsl"

layout(binding = 1) uniform sampler2D texSampler0;
layout(binding = 2) uniform sampler2D texSampler1;

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void main() {
  // outColor = vec4(fragColor, 1.0);
  outColor = mix(texture(texSampler0, fragTexCoord), texture(texSampler1, fragTexCoord), sin(sceneConstants.TIME) * 0.5 + 0.5);
  // outColor = vec4(fragColor * texture(texSampler, fragTexCoord * 2.0).rgb, 1.0);
}
