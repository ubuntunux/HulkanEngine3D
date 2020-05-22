#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(location = 0) in vec3 vs_input_position;
layout(location = 1) in vec3 vs_input_normal;
layout(location = 2) in vec4 vs_input_color;
layout(location = 3) in vec2 vs_input_texCoord;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    // TODO : VIEW_ORIGIN_PROJECTION
    vec4 projection_pos = viewProjectionConstants.VIEW_PROJECTION * pushConstant.localMatrix * vec4(vs_input_position, 1.0);
    gl_Position = projection_pos;

    vs_output.color = vs_input_color;
    vs_output.normal = (pushConstant.localMatrix * vec4(vs_input_normal, 0.0)).xyz;
    vs_output.texCoord = vs_input_texCoord;
}
