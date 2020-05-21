layout(binding = 0) uniform SceneConstants
{
    mat4 VIEW;
    mat4 PROJECTION;
    mat4 VIEW_PROJECTION;
    mat4 INV_VIEW_PROJECTION;
    float TIME;
    float SceneConstantsDummy0;
    float SceneConstantsDummy1;
    float SceneConstantsDummy2;
} sceneConstants;