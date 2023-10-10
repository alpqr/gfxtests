#version 440

layout(location = 0) out vec4 fragColor;

layout(std140, binding = 1) uniform buf {
    mat4 modelMatrix;
};

void main()
{
    fragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
