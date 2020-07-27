#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"
#include "PCFKernels.glsl"

#define DISTANCE_CHECK true

layout (constant_id = 0) const int SSAO_KERNEL_SIZE = 64;

layout(binding = 3) uniform sampler2D textureSceneNormal;
layout(binding = 4) uniform sampler2D textureSceneDepth;
layout(binding = 5) uniform sampler2D ssaoNoise;
layout(binding = 6) uniform UBOSSAOKernel
{
    vec4 _SSAO_KERNEL_SAPLES[SSAO_KERNEL_SIZE];
} uboSSAOKernel;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out float outColor;

void main() {
    const float device_depth = texture(textureSceneDepth, texCoord).x;
    if(1.0 == device_depth)
    {
        discard;
    }

    const float linear_depth = device_depth_to_linear_depth(device_depth);
    const vec4 relative_pos = relative_world_from_linear_depth(texCoord, linear_depth);
    const vec3 normal = texture(textureSceneNormal, texCoord).xyz;
    const vec2 texture_size = textureSize(textureSceneDepth, 0);
    const vec2 noise_size = textureSize(ssaoNoise, 0);
    const vec3 randomVec = normalize(vec3(texture(ssaoNoise, texCoord * texture_size / noise_size).xy, 0.0).xzy);

    vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
    const vec3 bitangent = normalize(cross(normal, tangent));
    const mat3 tnb = mat3(tangent, normal, bitangent);
    const vec2 occlusion_distance_min_max = vec2(0.1, 1.5);
    const float occlusion_density_min = 0.5;

    float occlusion = 0.0;
    const int sample_count = SSAO_KERNEL_SIZE;
    for (int i = 0; i < sample_count; ++i)
    {
        vec3 pos = (tnb * uboSSAOKernel._SSAO_KERNEL_SAPLES[i].xyz) * occlusion_distance_min_max.y + relative_pos.xyz;

        // project sample position:
        vec4 offset = vec4(pos, 1.0);
        offset = viewConstants.VIEW_ORIGIN_PROJECTION * offset;
        offset.xy /= offset.w;
        offset.xy = offset.xy * 0.5 + 0.5;

        if(offset.x < 0.0 || offset.x > 1.0 || offset.y < 0.0 || offset.y > 1.0)
        {
            continue;
        }

        const float occlusion_depth = texture(textureSceneDepth, offset.xy).x;
        const float occlusion_linear_depth = device_depth_to_linear_depth(occlusion_depth);

        if(offset.w < occlusion_linear_depth)
        {
            continue;
        }

        if(DISTANCE_CHECK)
        {
            const vec4 occlusion_relative_pos = relative_world_from_linear_depth(offset.xy, occlusion_linear_depth);
            const float distance = length(occlusion_relative_pos - relative_pos);
            const float weight = 1.0 - smoothstep(occlusion_distance_min_max.x, occlusion_distance_min_max.y, distance);
            occlusion += mix(occlusion_density_min, 1.0, weight);
        }
        else
        {
            occlusion += 1.0;
        }
    }

    occlusion = clamp(1.0 - occlusion / float(sample_count), 0.0, 1.0);
    outColor = saturate(occlusion * occlusion);
}