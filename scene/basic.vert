#version 440

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec3 normal;

layout(std140, binding = 0) uniform mainbuf {
    mat4 viewProjectionMatrix;
};

layout(std140, binding = 1) uniform buf {
    mat4 modelMatrix;
};

void main()
{
    gl_Position = viewProjectionMatrix * modelMatrix * position;
}
