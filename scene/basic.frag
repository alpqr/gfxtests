#version 440

layout(location = 0) in vec2 v_uv;

layout(location = 0) out vec4 fragColor;

layout(std140, binding = 1) uniform buf {
    mat4 modelMatrix;
    vec4 baseColorFactor;
};

layout(binding = 2) uniform sampler2D baseColorMap;

vec3 linearToSrgb(vec3 color)
{
    vec3 S1 = sqrt(color);
    vec3 S2 = sqrt(S1);
    vec3 S3 = sqrt(S2);
    return 0.585122381 * S1 + 0.783140355 * S2 - 0.368262736 * S3;
}

vec4 linearToSrgb(vec4 color)
{
    return vec4(linearToSrgb(color.rgb), color.a);
}

vec3 srgbToLinear(vec3 color)
{
    return color * (color * (color * 0.305306011 + 0.682171111) + 0.012522878);
}

vec4 srgbToLinear(vec4 color)
{
    return vec4(srgbToLinear(color.rgb), color.a);
}

void main()
{
    vec4 baseCol = srgbToLinear(baseColorFactor);
    vec4 baseTexCol = srgbToLinear(texture(baseColorMap, v_uv));
    vec4 c = baseCol * baseTexCol;
    fragColor = linearToSrgb(c);
}
