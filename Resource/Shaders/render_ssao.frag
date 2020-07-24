#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"

layout (constant_id = 0) const int SSAO_KERNEL_SIZE = 64;
layout (constant_id = 1) const float SSAO_RADIUS = 0.05;
layout (constant_id = 2) const float SSAO_POWER = 1.0;

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
    float linear_depth = device_depth_to_linear_depth(texture(textureSceneDepth, texCoord).x);
    vec4 relative_pos = relative_world_from_linear_depth(texCoord, linear_depth);
    vec3 normal = texture(textureSceneNormal, texCoord).xyz * 2.0 - 1.0;
    vec2 texture_size = textureSize(textureSceneDepth, 0);
    vec2 noise_size = textureSize(ssaoNoise, 0);

    vec3 randomVec = vec3(texture(ssaoNoise, texCoord * texture_size / noise_size).xy, 0.0).xzy;

    vec3 tangent   = normalize(randomVec - normal * dot(randomVec, normal));
    vec3 bitangent = cross(normal, tangent);
    mat3 tbn = mat3(tangent, normal, bitangent);

    vec2 radius_min_max = vec2(0.05, 1.5);

    float occlusion = 0.0;
    int sample_count = SSAO_KERNEL_SIZE;//min(16, SSAO_KERNEL_SIZE);
    int occlusionCount = 0;
    for (int i = 0; i < sample_count; ++i)
    {
        vec3 pos = (tbn * uboSSAOKernel._SSAO_KERNEL_SAPLES[i].xyz) * radius_min_max.y + relative_pos.xyz;

        // project sample position:
        vec4 offset = vec4(pos, 1.0);
        offset = viewConstants.VIEW_ORIGIN_PROJECTION * offset;
        offset.xy /= offset.w;
        offset.xy = offset.xy * 0.5 + 0.5;

        if(offset.x < 0.0 || offset.x > 1.0 || offset.y < 0.0 || offset.y > 1.0)
        {
            continue;
        }

        float sampleDepth = device_depth_to_linear_depth(texture(textureSceneDepth, offset.xy).x);
        sampleDepth = linear_depth - sampleDepth;
        if(radius_min_max.x <= sampleDepth && sampleDepth <= radius_min_max.y)
        {
            float weight = clamp(1.0 - (sampleDepth - radius_min_max.x) / (radius_min_max.y - radius_min_max.x), 0.0, 1.0);
            occlusion += pow(weight, 3.0);
        }
        ++occlusionCount;
    }
    occlusion = clamp(1.0 - occlusion / float(occlusionCount), 0.0, 1.0);
    outColor = saturate(1.0 - occlusion * occlusion);

//    // Get G-Buffer values
//    float depth = texture(textureSceneDepth, texCoord).x;
//    vec3 relativePos = relative_world_from_device_depth(texCoord, depth).xyz;
//    vec3 normal = texture(textureSceneNormal, texCoord).xyz;
//
//    // Get a random vector using a noise lookup
//    ivec2 texDim = textureSize(textureSceneDepth, 0);
//    ivec2 noiseDim = textureSize(ssaoNoise, 0);
//    const vec2 noiseUV = vec2(texDim) / vec2(noiseDim) * texCoord;
//    vec3 randomVec = normalize(vec3(texture(ssaoNoise, noiseUV).xy, 0.0)).xzy;
//
//    // Create TBN matrix
//    vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
//    vec3 bitangent = cross(normal, tangent);
//    mat3 TBN = mat3(tangent, normal, bitangent);
//
//    const float ssao_radius = 0.5;
//
//    // Calculate occlusion value
//    float occlusion = 0.0f;
//    for(int i = 0; i < SSAO_KERNEL_SIZE; i++)
//    {
//        vec3 samplePos = TBN * normalize(uboSSAOKernel._SSAO_KERNEL_SAPLES[i].xzy);
//        samplePos = relativePos + samplePos * ssao_radius;
//
//        // project
//        vec4 offset = viewConstants.VIEW_ORIGIN_PROJECTION * vec4(samplePos, 1.0f);
//        offset.xyz /= offset.w;
//        offset.xy = offset.xy * 0.5f + 0.5f;
//
//        float sampleDepth = texture(textureSceneDepth, offset.xy).x;
//        float sampleLinearDepth = device_depth_to_linear_depth(sampleDepth);
//
//        // Range check
//        vec3 viewDir = vec3(viewConstants.VIEW[0].z, viewConstants.VIEW[1].z, viewConstants.VIEW[2].z);
//        float rangeCheck = smoothstep(0.0f, 1.0f, ssao_radius / abs(dot(-viewDir, relativePos) - sampleLinearDepth));
//        occlusion += (sampleLinearDepth >= samplePos.z ? 1.0f : 0.0f) * rangeCheck;
//    }
//
//    occlusion = 1.0 - (occlusion / float(SSAO_KERNEL_SIZE));
//    occlusion = pow(occlusion, SSAO_POWER);
//    outColor = saturate(1.0 - occlusion);
}