layout(binding = 0) uniform SceneConstants
{
    vec2 SCREEN_SIZE;
    vec2 BACKBUFFER_SIZE;
    float TIME;
    float DELTA_TIME;
    float JITTER_FRAME;
    int SceneConstantsDummy0;
} sceneConstants;

layout(binding = 1) uniform ViewProjectionConstants
{
    mat4 VIEW;
    mat4 PROJECTION;
    mat4 VIEW_PROJECTION;
    mat4 INV_VIEW_PROJECTION;
} viewProjectionConstants;

layout(binding = 2) uniform LightConstants
{
    mat4 SHADOW_VIEW_PROJECTION;
    vec3 LIGHT_POSITION;
    float SHADOW_EXP;
    vec3 LIGHT_DIRECTION;
    float SHADOW_BIAS;
    vec3 LIGHT_COLOR;
    int SHADOW_SAMPLES;
} lightConstants;
