#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    // TODO : VIEW_ORIGIN_PROJECTION
    vec4 projection_pos = viewProjectionConstants.VIEW_PROJECTION * pushConstant.localMatrix * vec4(inPosition, 1.0);
    gl_Position = projection_pos;

    vs_output.color = inColor;
    vec3 bitangent = cross(inTangent, inNormal);

    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vs_output.tangent_to_world = mat3(pushConstant.localMatrix) * mat3(inTangent, bitangent, inNormal);

    vs_output.texCoord = vec2(inTexCoord.x, 1.0 - inTexCoord.y);
}
