// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QApplication>
#include <QCommandLineParser>
#include <QElapsedTimer>
#include <QRhiWidget>
#include <QKeyEvent>
#include <QQuaternion>
#include <QFile>
#include <QFileInfo>
#include <rhi/qrhi.h>
#include <thread>
#include <mutex>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#define CGLTF_IMPLEMENTATION
#include "../3rdparty/cgltf/cgltf.h"

#include "qrhiimgui.h"
#include "imgui.h"

struct Transform
{
    QVector3D position;
    QVector3D scale = { 1, 1, 1 };
    QVector3D pivot;
    QVector3D eulerRotation; // in degrees

    void updateRotation();
    QQuaternion rotation;

    void update();
    QMatrix4x4 matrix;
    QMatrix3x3 normalMatrix;

    QVector3D mapDirection(const QVector3D &localDirection) const;
    QVector3D forward() const { return mapDirection(QVector3D(0, 0, -1)).normalized(); }
    QVector3D up() const { return mapDirection(QVector3D(0, 1, 0)).normalized(); }
    QVector3D right() const { return mapDirection(QVector3D(1, 0, 0)).normalized(); }
};

void Transform::updateRotation()
{
    rotation = QQuaternion::fromEulerAngles(eulerRotation).normalized();
}

void Transform::update()
{
    matrix.setToIdentity();

    const QVector3D offset = (-pivot * scale);

    matrix(0, 0) = scale[0];
    matrix(1, 1) = scale[1];
    matrix(2, 2) = scale[2];

    matrix(0, 3) = offset[0];
    matrix(1, 3) = offset[1];
    matrix(2, 3) = offset[2];

    matrix = QMatrix4x4(rotation.toRotationMatrix()) * matrix;

    matrix(0, 3) += position[0];
    matrix(1, 3) += position[1];
    matrix(2, 3) += position[2];

    normalMatrix = matrix.normalMatrix();
}

static inline QVector3D transformVecByMat33(const QMatrix3x3 &m, const QVector3D &v)
{
    const QVector3D c0 = QVector3D(m(0, 0), m(1, 0), m(2, 0));
    const QVector3D c1 = QVector3D(m(0, 1), m(1, 1), m(2, 1));
    const QVector3D c2 = QVector3D(m(0, 2), m(1, 2), m(2, 2));
    return c0 * v.x() + c1 * v.y() + c2 * v.z();
}

QVector3D Transform::mapDirection(const QVector3D &localDirection) const
{
    return transformVecByMat33(normalMatrix, localDirection);
}

struct Camera
{
    void updateProjection(const QMatrix4x4 &clipSpaceCorrMatrix, float fovDegrees, float aspectRatio,
                          float clipNear, float clipFar);
    void updateViewProjection();

    Transform worldTransform;
    QMatrix4x4 projection;
    QMatrix4x4 viewProjection;
};

void Camera::updateProjection(const QMatrix4x4 &clipSpaceCorrMatrix, float fovDegrees, float aspectRatio,
                              float clipNear, float clipFar)
{
    projection = clipSpaceCorrMatrix;
    projection.perspective(fovDegrees, aspectRatio, clipNear, clipFar);
}

void Camera::updateViewProjection()
{
    QMatrix4x4 m(Qt::Uninitialized);
    m.setColumn(0, worldTransform.matrix.column(0).normalized());
    m.setColumn(1, worldTransform.matrix.column(1).normalized());
    m.setColumn(2, worldTransform.matrix.column(2).normalized());
    m.setColumn(3, worldTransform.matrix.column(3));
    viewProjection = projection * m.inverted();
}

struct WASDController
{
    WASDController(Camera *camera) : camera(camera) { }
    void update();

    float moveSpeed = 3.0f;
    float xLookSpeed = 0.05f;
    float yLookSpeed = 0.05f;

    void handleKeyPress(int key);
    void handleKeyRelease(int key);
    void handleMousePress(const QPointF &pos);
    void handleMouseRelease();
    void handleMouseMove(const QPointF &pos);

    Camera *camera;
    struct {
        bool moveForward, moveBack, moveLeft, moveRight, moveUp, moveDown;
        bool moveMouse;
        QPointF currentMousePos;
        QPointF lastMousePos;
        QElapsedTimer delta;
        bool deltaValid;
    } state = {};
};

void WASDController::update()
{
    float delta = 16.6f;
    if (state.deltaValid) {
        delta = state.delta.restart();
    } else {
        state.delta.start();
        state.deltaValid = true;
    }
    delta = std::min(33.3f, std::max(0.0f, delta));

    if (state.moveForward || state.moveBack || state.moveLeft || state.moveRight || state.moveUp || state.moveDown) {
        camera->worldTransform.update();
        const float speed = moveSpeed * delta;
        QVector3D direction;
        if (state.moveForward || state.moveBack) {
            if (state.moveForward)
                direction = camera->worldTransform.forward();
            else
                direction = -camera->worldTransform.forward();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->worldTransform.position += velocity;
        }
        if (state.moveRight || state.moveLeft) {
            if (state.moveRight)
                direction = camera->worldTransform.right();
            else
                direction = -camera->worldTransform.right();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->worldTransform.position += velocity;
        }
        if (state.moveUp || state.moveDown) {
            if (state.moveUp)
                direction = camera->worldTransform.up();
            else
                direction = -camera->worldTransform.up();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->worldTransform.position += velocity;
        }
    }

    if (state.moveMouse) {
        QVector3D r = camera->worldTransform.eulerRotation;
        float rx = (state.lastMousePos.x() - state.currentMousePos.x()) * xLookSpeed * delta;
        r.setY(r.y() + rx);

        float ry = -1.0f * ((state.lastMousePos.y() - state.currentMousePos.y()) * -yLookSpeed * delta);
        r.setX(r.x() + ry);

        camera->worldTransform.eulerRotation = r;
        camera->worldTransform.updateRotation();

        state.lastMousePos = state.currentMousePos;
    }
}

void WASDController::handleKeyPress(int key)
{
    switch (key) {
    case Qt::Key_W:
        state.moveForward = true;
        state.moveBack = false;
        break;
    case Qt::Key_S:
        state.moveBack = true;
        state.moveForward = false;
        break;
    case Qt::Key_D:
        state.moveRight = true;
        state.moveLeft = false;
        break;
    case Qt::Key_A:
        state.moveLeft = true;
        state.moveRight = false;
        break;
    case Qt::Key_R:
        state.moveUp = true;
        state.moveDown = false;
        break;
    case Qt::Key_F:
        state.moveDown = true;
        state.moveUp = false;
        break;
    }
}

void WASDController::handleKeyRelease(int key)
{
    switch (key) {
    case Qt::Key_W:
        state.moveForward = false;
        break;
    case Qt::Key_S:
        state.moveBack = false;
        break;
    case Qt::Key_D:
        state.moveRight = false;
        break;
    case Qt::Key_A:
        state.moveLeft = false;
        break;
    case Qt::Key_R:
        state.moveUp = false;
        break;
    case Qt::Key_F:
        state.moveDown = false;
        break;
    }
}

void WASDController::handleMousePress(const QPointF &pos)
{
    state.currentMousePos = pos;
    state.lastMousePos = pos;
    state.moveMouse = true;
}

void WASDController::handleMouseRelease()
{
    state.moveMouse = false;
}

void WASDController::handleMouseMove(const QPointF &pos)
{
    state.currentMousePos = pos;
}

struct Model
{
    struct SubMesh {
        quint32 indexByteOffset = 0;
        quint32 indexCount = 0;
        quint32 vertexByteOffset = 0;
        std::optional<QVector3D> minPos;
        std::optional<QVector3D> maxPos;
        std::optional<qsizetype> materialIndex;
        QRhiShaderResourceBindings *srb = nullptr;
    };

    struct Mesh {
        QVector<SubMesh> subMeshes;
    };

    bool load(const QString &filename, const QString &key);
    void reset();

    QVector<Mesh> meshes;
    QString sourceFilename;
    QString key;
};

struct ModelInstance
{
    qsizetype modelIndex;
    Transform worldTransform;
};

struct R
{
    struct Image {
        QImage image;
        QString sourceFilename;
        qsizetype globalGltfIndex;
    };

    struct TextureMap {
        qsizetype imageIndex;
        QRhiTexture *texture;
    };

    struct Material {
        std::optional<TextureMap> baseColorMap;
        QVector4D baseColorFactor;
    };

    void init();
    void reset();
    quint32 indexStorage(quint32 byteSize);
    quint32 vertexStorage(quint32 byteSize);
    void createTextures(QRhiCommandBuffer *cb);

    QRhi *rhi = nullptr;
    QVector<Model> models;
    QVector<ModelInstance> modelInstances;
    quint32 totalSubMeshCount = 0;
    quint32 vertexByteStride;
    QByteArray indices;
    QByteArray vertices;
    QVector<Image> images;
    QVector<Material> materials;
    Camera camera;
    QRhiTexture *dummyTexture = nullptr;

private:
    void createTexture(TextureMap *t, QRhiResourceUpdateBatch *u);
} r;

void R::init()
{
    indices.reserve(32 * 1024 * 1024);
    vertices.reserve(256 * 1024 * 1024);

    // position, UV, normal
    vertexByteStride = (3 + 2 + 3) * sizeof(float);
}

void Model::reset()
{
    for (Model::Mesh &mesh : meshes) {
        for (Model::SubMesh &subMesh : mesh.subMeshes) {
            delete subMesh.srb;
            subMesh.srb = nullptr;
        }
    }
    meshes.clear();
}

void R::reset()
{
    indices.clear();
    vertices.clear();
    images.clear();
    for (Material &mat : materials) {
        if (mat.baseColorMap.has_value()) {
            delete mat.baseColorMap->texture;
            mat.baseColorMap->texture = {};
        }
    }
    for (Model &model : models)
        model.reset();
    models.clear();
    modelInstances.clear();
    totalSubMeshCount = 0;
    materials.clear();
    delete dummyTexture;
    dummyTexture = nullptr;
}

quint32 R::indexStorage(quint32 byteSize)
{
    const quint32 result = indices.size();
    indices.resize(indices.size() + byteSize);
    return result;
}

quint32 R::vertexStorage(quint32 byteSize)
{
    const quint32 result = vertices.size();
    vertices.resize(vertices.size() + byteSize);
    return result;
}

void R::createTexture(TextureMap *t, QRhiResourceUpdateBatch *u)
{
    const QImage &image(images[t->imageIndex].image);
    QRhiTexture *texture = rhi->newTexture(QRhiTexture::RGBA8, image.size());
    texture->create();
    u->uploadTexture(texture, image.convertToFormat(QImage::Format_RGBA8888));
    t->texture = texture;
}

void R::createTextures(QRhiCommandBuffer *cb)
{
    QElapsedTimer timer;
    timer.start();
    QRhiResourceUpdateBatch *u = rhi->nextResourceUpdateBatch();
    for (Material &mat : r.materials) {
        if (mat.baseColorMap.has_value())
            r.createTexture(&mat.baseColorMap.value(), u);
    }

    dummyTexture = rhi->newTexture(QRhiTexture::RGBA8, QSize(16, 16));
    dummyTexture->create();
    QImage dummyContent(dummyTexture->pixelSize(), QImage::Format_RGBA8888);
    dummyContent.fill(Qt::white);
    u->uploadTexture(dummyTexture, dummyContent);

    cb->resourceUpdate(u);
    qDebug() << "Texture uploads" << timer.elapsed() << "ms";
}

bool Model::load(const QString &filename, const QString &key)
{
    cgltf_data *data = nullptr;

    const QByteArray fn = filename.toUtf8();
    cgltf_options options = {};
    cgltf_result result = cgltf_parse_file(&options, fn.constData(), &data);
    if (result != cgltf_result_success) {
        qWarning("Failed to parse glTF file: %d", int(result));
        return false;
    }
    result = cgltf_load_buffers(&options, data, fn.constData());
    if (result != cgltf_result_success) {
        qWarning("Failed to load buffers: %d", int(result));
        cgltf_free(data);
        data = nullptr;
        return false;
    }
    sourceFilename = filename;
    this->key = key;

    const QString prefix = QFileInfo(filename).absolutePath() + QLatin1Char('/');
    QStringList imagePathList;
    for (cgltf_size i = 0; i < data->images_count; ++i) {
        const cgltf_image *image = &data->images[i];
        const QString fn = prefix + QString::fromUtf8(image->uri);
        imagePathList.append(fn);
    }

    std::mutex mutex;
    auto loadImageFunc = [&mutex](const QString &fn, qsizetype globalGltfIndex) {
        QImage img;
        img.load(fn);
        if (img.isNull()) {
            qWarning() << "Failed to load" << fn;
            img = QImage(16, 16, QImage::Format_RGBA8888);
            img.fill(Qt::magenta);
        }
        {
            std::lock_guard<std::mutex> guard(mutex);
            r.images.append({ img, fn, globalGltfIndex });
        }
    };

    const int threadCount = std::thread::hardware_concurrency();
    const qsizetype firstImageGlobalIndex = r.images.count();
    for (qsizetype t = 0; t < imagePathList.count(); t += threadCount) {
        const int n = std::min<int>(imagePathList.count() - t, threadCount);
        QVarLengthArray<std::thread *, 16> threadList;
        for (int i = 0; i < n; ++i) {
            const qsizetype gltfIndex = t + i;
            const QString fn = imagePathList[gltfIndex];
            threadList.append(new std::thread(std::bind(loadImageFunc, fn, firstImageGlobalIndex + gltfIndex)));
        }
        for (std::thread *thread : threadList)
            thread->join();
        qDeleteAll(threadList);
    }

    const qsizetype firstMaterialGlobalIndex = r.materials.count();
    for (cgltf_size i = 0; i < data->materials_count; ++i) {
        const cgltf_material *gmat = &data->materials[i];
        R::Material mat;
        if (gmat->has_pbr_metallic_roughness) {
            const cgltf_texture_view *baseColorView = &gmat->pbr_metallic_roughness.base_color_texture;
            if (baseColorView->texture) {
                R::TextureMap t;
                const cgltf_size globalGltfIndex = firstImageGlobalIndex + cgltf_image_index(data, baseColorView->texture->image);
                bool mapped = false;
                for (qsizetype localImageIndex = 0; localImageIndex < imagePathList.count(); ++localImageIndex) {
                    const qsizetype globalImageIndex = firstImageGlobalIndex + localImageIndex;
                    if (r.images[globalImageIndex].globalGltfIndex == globalGltfIndex) {
                        t.imageIndex = globalImageIndex;
                        mapped = true;
                        break;
                    }
                }
                if (!mapped) {
                    qWarning() << "Failed to find image for gltf image index" << firstImageGlobalIndex
                               << " + " << cgltf_image_index(data, baseColorView->texture->image);
                }
                mat.baseColorMap = t;
            }
            mat.baseColorFactor = QVector4D(gmat->pbr_metallic_roughness.base_color_factor[0],
                                            gmat->pbr_metallic_roughness.base_color_factor[1],
                                            gmat->pbr_metallic_roughness.base_color_factor[2],
                                            gmat->pbr_metallic_roughness.base_color_factor[3]);
        } else {
            qWarning("Material %d is not PBR", int(i));
        }
        r.materials.append(mat);
    }

    cgltf_size totalIndexCount = 0;
    cgltf_size totalVertexCount = 0;
    for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        for (cgltf_size j = 0; j < data->meshes[i].primitives_count; ++j) {
            totalIndexCount += data->meshes[i].primitives[j].indices->count;
            for (cgltf_size a = 0; a < data->meshes[i].primitives[j].attributes_count; ++a)
                totalVertexCount += data->meshes[i].primitives[j].attributes[a].data->count;
        }
    }

    quint32 currentIndexByteOffset = r.indexStorage(totalIndexCount * sizeof(quint32));

    QVector<QVector3D> positions;
    QVector<QVector2D> uvs;
    QVector<QVector3D> normals;
    positions.reserve(totalVertexCount);
    uvs.reserve(totalVertexCount);
    normals.reserve(totalVertexCount);

    meshes.resize(data->meshes_count);
    for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        const cgltf_mesh *mesh = &data->meshes[i];
        meshes[i].subMeshes.reserve(mesh->primitives_count);

        for (cgltf_size j = 0; j < mesh->primitives_count; ++j) {
            const cgltf_primitive *primitive = &mesh->primitives[j];
            SubMesh subMesh;
            const qsizetype subMeshVertexIndex = positions.count();

            for (cgltf_size a = 0; a < primitive->attributes_count; ++a) {
                const cgltf_attribute *attrib = &primitive->attributes[a];
                const cgltf_accessor *accessor = attrib->data;
                const cgltf_buffer_view *view = accessor->buffer_view;
                const cgltf_size stride = view->stride ? view->stride : accessor->stride;
                const quint8 *src = static_cast<const quint8 *>(view->buffer->data) + accessor->offset + view->offset;

                if (attrib->type == cgltf_attribute_type_position) {
                    if (accessor->has_min)
                        subMesh.minPos = QVector3D(accessor->min[0], accessor->min[1], accessor->min[2]);
                    if (accessor->has_max)
                        subMesh.maxPos = QVector3D(accessor->max[0], accessor->max[1], accessor->max[2]);
                }

                for (cgltf_size vertexIndex = 0; vertexIndex < accessor->count; ++vertexIndex) {
                    if (attrib->type == cgltf_attribute_type_position) {
                        if (accessor->type == cgltf_type_vec3) {
                            const float *p = reinterpret_cast<const float *>(src);
                            positions.append(QVector3D(p[0], p[1], p[2]));
                        } else {
                            positions.append(QVector3D());
                        }
                    } else if (attrib->type == cgltf_attribute_type_texcoord && attrib->index == 0) {
                        if (accessor->type == cgltf_type_vec2) {
                            const float *p = reinterpret_cast<const float *>(src);
                            uvs.append(QVector2D(p[0], p[1]));
                        } else {
                            uvs.append(QVector2D());
                        }
                    } else if (attrib->type == cgltf_attribute_type_normal) {
                        if (accessor->type == cgltf_type_vec3) {
                            const float *p = reinterpret_cast<const float *>(src);
                            normals.append(QVector3D(p[0], p[1], p[2]));
                        } else {
                            normals.append(QVector3D());
                        }
                    }
                    src += stride;
                }
            }
            uvs.resize(positions.size());
            normals.resize(positions.size());

            subMesh.vertexByteOffset = subMeshVertexIndex * r.vertexByteStride;

            const cgltf_accessor *indexAccessor = primitive->indices;
            subMesh.indexByteOffset = currentIndexByteOffset;
            subMesh.indexCount = indexAccessor->count;
            const quint8 *addr = static_cast<const quint8 *>(indexAccessor->buffer_view->buffer->data) + indexAccessor->buffer_view->offset + indexAccessor->offset;
            quint32 *dst = reinterpret_cast<quint32 *>(r.indices.data() + currentIndexByteOffset);
            if (indexAccessor->stride == 1) {
                for (cgltf_size a = 0; a < indexAccessor->count; ++a)
                    *dst++ = addr[a];
            } else if (indexAccessor->stride == 2) {
                const quint16 *p = reinterpret_cast<const quint16 *>(addr);
                for (cgltf_size a = 0; a < indexAccessor->count; ++a)
                    *dst++ = p[a];
            } else if (indexAccessor->stride == 4) {
                const quint32 *p = reinterpret_cast<const quint32 *>(addr);
                for (cgltf_size a = 0; a < indexAccessor->count; ++a)
                    *dst++ = p[a];
            }
            currentIndexByteOffset += indexAccessor->count * sizeof(quint32);

            if (primitive->material)
                subMesh.materialIndex = firstMaterialGlobalIndex + cgltf_material_index(data, primitive->material);

            meshes[i].subMeshes.append(subMesh);
        }
    }

    const quint32 dstOffset = r.vertexStorage(positions.count() * r.vertexByteStride);
    for (Model::Mesh &mesh : meshes) {
        for (Model::SubMesh &subMesh : mesh.subMeshes)
            subMesh.vertexByteOffset += dstOffset;
    }
    float *vp = reinterpret_cast<float *>(r.vertices.data() + dstOffset);
    for (qsizetype i = 0, count = positions.count(); i < count; ++i) {
        memcpy(vp, &positions[i], 3 * sizeof(float));
        memcpy(vp + 3, &uvs[i], 5 * sizeof(float));
        memcpy(vp + 5, &normals[i], 3 * sizeof(float));
        vp += 8;
    }

    cgltf_free(data);
    return true;
}

static QShader getShader(const QString &name)
{
    QFile f(name);
    return f.open(QIODevice::ReadOnly) ? QShader::fromSerialized(f.readAll()) : QShader();
}

class ExampleRhiWidget : public QRhiWidget
{
public:
    ExampleRhiWidget(QWidget *parent = nullptr);
    void initialize(QRhiCommandBuffer *cb) override;
    void render(QRhiCommandBuffer *cb) override;
    void keyPressEvent(QKeyEvent *event) override;
    void keyReleaseEvent(QKeyEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    bool event(QEvent *e) override;

private:
    void gui();
    enum class Pipeline {
        Default,
        Wireframe
    };
    QRhiGraphicsPipeline *createPipeline(Pipeline which);

    WASDController m_wasd;
    QRhi *m_rhi = nullptr;
    std::unique_ptr<QRhiBuffer> m_vbuf;
    std::unique_ptr<QRhiBuffer> m_ibuf;
    std::unique_ptr<QRhiBuffer> m_mainUniformBuffer;
    std::unique_ptr<QRhiBuffer> m_renderableUniformBuffer;
    std::unique_ptr<QRhiSampler> m_sampler;
    std::unique_ptr<QRhiShaderResourceBindings> m_srbLayout;
    std::unique_ptr<QRhiGraphicsPipeline> m_pipeline;
    std::unique_ptr<QRhiGraphicsPipeline> m_pipelineWireframe;
    quint32 m_renderableUbufSize;
    quint32 m_renderableAlloc;
    std::unique_ptr<QRhiImguiRenderer> m_imguiRenderer;
    QRhiImgui m_imgui;
    QMatrix4x4 m_guiMvp;
    bool m_guiVisible = false;
    bool m_wireframe = false;
    double m_lastGpuTimeSec = 0;
    QElapsedTimer m_cpuFrameTimer;
};

ExampleRhiWidget::ExampleRhiWidget(QWidget *parent)
    : QRhiWidget(parent),
      m_wasd(&r.camera)
{
    setMouseTracking(true);
}

QRhiGraphicsPipeline *ExampleRhiWidget::createPipeline(Pipeline which)
{
    QRhiGraphicsPipeline *ps = m_rhi->newGraphicsPipeline();
    ps->setShaderStages({
        { QRhiShaderStage::Vertex, getShader(QLatin1String(":/basic.vert.qsb")) },
        { QRhiShaderStage::Fragment, getShader(QLatin1String(":/basic.frag.qsb")) }
    });
    ps->setDepthTest(true);
    ps->setDepthWrite(true);
    ps->setCullMode(QRhiGraphicsPipeline::Back);
    if (which == Pipeline::Wireframe)
        ps->setPolygonMode(QRhiGraphicsPipeline::Line);
    QRhiVertexInputLayout inputLayout;
    inputLayout.setBindings({
        { 8 * sizeof(float) }
    });
    inputLayout.setAttributes({
        { 0, 0, QRhiVertexInputAttribute::Float3, 0 },
        { 0, 1, QRhiVertexInputAttribute::Float2, 3 * sizeof(float) },
        { 0, 2, QRhiVertexInputAttribute::Float3, 5 * sizeof(float) }
    });
    ps->setVertexInputLayout(inputLayout);
    ps->setShaderResourceBindings(m_srbLayout.get());
    ps->setRenderPassDescriptor(renderTarget()->renderPassDescriptor());
    ps->create();
    return ps;
}

static const quint32 MAIN_UBUF_SIZE = 64;
static const quint32 RENDERABLE_UBUF_SIZE = 64 + 16;

void ExampleRhiWidget::initialize(QRhiCommandBuffer *cb)
{
    if (!r.rhi) {
        m_rhi = rhi();
        r.rhi = m_rhi;
        r.rhi->addCleanupCallback([](QRhi*) { r.reset(); });
        r.createTextures(cb);
    }

    if (!m_imguiRenderer) {
        ImGuiIO &io(ImGui::GetIO());
        io.IniFilename = nullptr;
        m_imgui.rebuildFontAtlasWithFont(QLatin1String(":/RobotoMono-Medium.ttf"));
        m_imguiRenderer.reset(new QRhiImguiRenderer);
    }

    if (!m_pipeline) {
        m_vbuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::VertexBuffer, r.vertices.size()));
        m_vbuf->create();

        m_ibuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::IndexBuffer, r.indices.size()));
        m_ibuf->create();

        m_mainUniformBuffer.reset(m_rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, MAIN_UBUF_SIZE));
        m_mainUniformBuffer->create();

        m_renderableAlloc = r.totalSubMeshCount;
        m_renderableUbufSize = m_rhi->ubufAligned(RENDERABLE_UBUF_SIZE);
        m_renderableUniformBuffer.reset(m_rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, m_renderableUbufSize * m_renderableAlloc));
        m_renderableUniformBuffer->create();

        m_sampler.reset(m_rhi->newSampler(QRhiSampler::Linear, QRhiSampler::Linear, QRhiSampler::None,
                                          QRhiSampler::Repeat, QRhiSampler::Repeat, QRhiSampler::Repeat));
        m_sampler->create();

        m_srbLayout.reset(m_rhi->newShaderResourceBindings());
        m_srbLayout->setBindings({
            QRhiShaderResourceBinding::uniformBuffer(0, QRhiShaderResourceBinding::VertexStage, nullptr),
            QRhiShaderResourceBinding::uniformBufferWithDynamicOffset(1, QRhiShaderResourceBinding::VertexStage | QRhiShaderResourceBinding::FragmentStage, nullptr, RENDERABLE_UBUF_SIZE),
            QRhiShaderResourceBinding::sampledTexture(2, QRhiShaderResourceBinding::FragmentStage, nullptr, nullptr)
        });
        m_srbLayout->create();

        m_pipeline.reset(createPipeline(Pipeline::Default));
        m_pipelineWireframe.reset(createPipeline(Pipeline::Wireframe));

        QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();
        resourceUpdates->uploadStaticBuffer(m_vbuf.get(), r.vertices.constData());
        resourceUpdates->uploadStaticBuffer(m_ibuf.get(), r.indices.constData());
        cb->resourceUpdate(resourceUpdates);
    }

    const QSize outputSize = colorTexture()->pixelSize();
    r.camera.updateProjection(m_rhi->clipSpaceCorrMatrix(), 60.0f, outputSize.width() / (float) outputSize.height(), 10.0f, 10000.0f);

    m_guiMvp = m_rhi->clipSpaceCorrMatrix();
    const float dpr = devicePixelRatio();
    m_guiMvp.ortho(0, outputSize.width() / dpr, outputSize.height() / dpr, 0, 1, -1);

    m_cpuFrameTimer.start();
}

void ExampleRhiWidget::render(QRhiCommandBuffer *cb)
{
    m_lastGpuTimeSec = cb->lastCompletedGpuTime();
    m_imgui.nextFrame(size(), devicePixelRatio(), QPointF(0, 0), std::bind(&ExampleRhiWidget::gui, this));
    m_imgui.syncRenderer(m_imguiRenderer.get());

    m_imguiRenderer->prepare(m_rhi, renderTarget(), cb, m_guiMvp, 1.0f);

    QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();

    m_wasd.update();
    r.camera.worldTransform.update();
    r.camera.updateViewProjection();

    resourceUpdates->updateDynamicBuffer(m_mainUniformBuffer.get(), 0, 64, r.camera.viewProjection.constData());

    if (m_renderableAlloc < r.totalSubMeshCount) {
        m_renderableAlloc = r.totalSubMeshCount;
        m_renderableUniformBuffer->setSize(m_renderableUbufSize * m_renderableAlloc);
        m_renderableUniformBuffer->create();
    }

    quint32 globalSubMeshIndex = 0;
    for (ModelInstance &modelInstance : r.modelInstances) {
        modelInstance.worldTransform.update();
        Model &model(r.models[modelInstance.modelIndex]);
        for (Model::Mesh &mesh : model.meshes) {
            for (Model::SubMesh &subMesh : mesh.subMeshes) {
                quint32 offset = globalSubMeshIndex * m_renderableUbufSize;
                resourceUpdates->updateDynamicBuffer(m_renderableUniformBuffer.get(), offset, 64, modelInstance.worldTransform.matrix.constData());
                QVector4D baseColorFactor = { 1, 1, 1, 1 };
                if (subMesh.materialIndex.has_value()) {
                    const R::Material &material(r.materials[subMesh.materialIndex.value()]);
                    baseColorFactor = material.baseColorFactor;
                }
                resourceUpdates->updateDynamicBuffer(m_renderableUniformBuffer.get(), offset + 64, 16, &baseColorFactor);
                globalSubMeshIndex += 1;
            }
        }
    }

    const QColor clearColor = QColor::fromRgbF(0.4f, 0.7f, 0.0f, 1.0f);
    cb->beginPass(renderTarget(), clearColor, { 1.0f, 0 }, resourceUpdates);

    if (m_wireframe)
        cb->setGraphicsPipeline(m_pipelineWireframe.get());
    else
        cb->setGraphicsPipeline(m_pipeline.get());

    const QSize outputSize = colorTexture()->pixelSize();
    cb->setViewport(QRhiViewport(0, 0, outputSize.width(), outputSize.height()));
    globalSubMeshIndex = 0;
    for (ModelInstance &modelInstance : r.modelInstances) {
        Model &model(r.models[modelInstance.modelIndex]);
        for (Model::Mesh &mesh : model.meshes) {
            for (Model::SubMesh &subMesh : mesh.subMeshes) {
                if (!subMesh.srb) {
                    QRhiTexture *baseColorMap = nullptr;
                    if (subMesh.materialIndex.has_value()) {
                        const R::Material &material(r.materials[subMesh.materialIndex.value()]);
                        if (material.baseColorMap.has_value())
                            baseColorMap = material.baseColorMap->texture;
                    }
                    if (!baseColorMap)
                        baseColorMap = r.dummyTexture;

                    subMesh.srb = m_rhi->newShaderResourceBindings();
                    subMesh.srb->setBindings({
                        QRhiShaderResourceBinding::uniformBuffer(0, QRhiShaderResourceBinding::VertexStage, m_mainUniformBuffer.get()),
                        QRhiShaderResourceBinding::uniformBufferWithDynamicOffset(1, QRhiShaderResourceBinding::VertexStage | QRhiShaderResourceBinding::FragmentStage, m_renderableUniformBuffer.get(), RENDERABLE_UBUF_SIZE),
                        QRhiShaderResourceBinding::sampledTexture(2, QRhiShaderResourceBinding::FragmentStage, baseColorMap, m_sampler.get())
                    });
                    subMesh.srb->create();
                }

                const QRhiCommandBuffer::DynamicOffset dynamicOffsets[] = { { 1, globalSubMeshIndex * m_renderableUbufSize } };
                cb->setShaderResources(subMesh.srb, std::size(dynamicOffsets), dynamicOffsets);
                const QRhiCommandBuffer::VertexInput vbufBinding(m_vbuf.get(), subMesh.vertexByteOffset);
                cb->setVertexInput(0, 1, &vbufBinding, m_ibuf.get(), subMesh.indexByteOffset, QRhiCommandBuffer::IndexUInt32);
                cb->drawIndexed(subMesh.indexCount);
                globalSubMeshIndex += 1;
            }
        }
    }

    m_imguiRenderer->render();

    cb->endPass();

    update();

    m_cpuFrameTimer.restart();
}

void ExampleRhiWidget::gui()
{
    if (!m_guiVisible)
        return;

    ImGui::SetNextWindowPos(ImVec2(10, 10), ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSize(ImVec2(0, 0), ImGuiCond_FirstUseEver);
    ImGui::Begin("Controls");

    ImGui::Checkbox("Wireframe", &m_wireframe);
    ImGui::Text("CPU render-to-render: %.3f ms", m_cpuFrameTimer.nsecsElapsed() / 1000000.0);
    ImGui::Text("GPU last completed frame: %.3f ms", m_lastGpuTimeSec * 1000.0);

    ImGui::End();
}

void ExampleRhiWidget::keyPressEvent(QKeyEvent *event)
{
    if (event->key() == Qt::Key_QuoteLeft || event->key() == Qt::Key_F9)
        m_guiVisible = !m_guiVisible;

    m_wasd.handleKeyPress(event->key());
}

void ExampleRhiWidget::keyReleaseEvent(QKeyEvent *event)
{
    m_wasd.handleKeyRelease(event->key());
}

void ExampleRhiWidget::mousePressEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton)
        m_wasd.handleMousePress(event->position());
}

void ExampleRhiWidget::mouseReleaseEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton)
        m_wasd.handleMouseRelease();
}

void ExampleRhiWidget::mouseMoveEvent(QMouseEvent *event)
{
    if (event->buttons().testFlag(Qt::LeftButton))
        m_wasd.handleMouseMove(event->position());
}

bool ExampleRhiWidget::event(QEvent *e)
{
    m_imgui.processEvent(e);
    return QRhiWidget::event(e);
}

struct InputScene
{
    struct Transform {
        QVector3D position;
        QVector3D rotation;
        QVector3D scale;
    };
    struct Model {
        QString key;
        QString source;
    };
    struct ModelInstance {
        QString key;
        Transform transform;
    };
    QVector<Model> models;
    QVector<ModelInstance> modelInstances;
    Transform camera;
};

static QVector3D parseVec3(const QJsonObject &obj, QLatin1StringView name, const QVector3D &defaultValue = {})
{
    const QJsonArray v = obj[name].toArray();
    return v.count() >= 3 ? QVector3D(v.at(0).toDouble(), v.at(1).toDouble(), v.at(2).toDouble()) : defaultValue;
}

static bool parseScene(InputScene *s)
{
    QFile f(QLatin1String("scene.json"));
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qWarning() << "Failed to open" << f.fileName();
        return 1;
    }
    QJsonParseError jsonError;
    const QJsonDocument doc = QJsonDocument::fromJson(f.readAll(), &jsonError);
    f.close();
    if (doc.isNull()) {
        qWarning() << "Failed to parse scene:" << jsonError.errorString() << "at offset" << jsonError.offset;
        return false;
    }

    const QJsonObject root = doc.object();

    const QJsonArray models = root["models"].toArray();
    for (qsizetype modelIndex = 0; modelIndex < models.count(); ++modelIndex) {
        const QJsonObject model = models[modelIndex].toObject();
        InputScene::Model m;
        m.key = model["key"].toString();
        m.source = model["source"].toString();
        if (!m.key.isEmpty() && !m.source.isEmpty())
            s->models.append(m);
    }

    const QJsonArray instances = root["modelInstances"].toArray();
    for (qsizetype instanceIndex = 0; instanceIndex < instances.count(); ++instanceIndex) {
        const QJsonObject instance = instances[instanceIndex].toObject();
        InputScene::ModelInstance mi;
        mi.key = instance["key"].toString();
        if (!mi.key.isEmpty()) {
            mi.transform.position = parseVec3(instance, QLatin1StringView("position"));
            mi.transform.rotation = parseVec3(instance, QLatin1StringView("rotation"));
            mi.transform.scale = parseVec3(instance, QLatin1StringView("scale"), { 1, 1, 1 });
            s->modelInstances.append(mi);
        }
    }

    const QJsonObject camera = root["camera"].toObject();
    s->camera.position = parseVec3(camera, QLatin1StringView("position"));
    s->camera.rotation = parseVec3(camera, QLatin1StringView("rotation"));
    s->camera.scale = parseVec3(camera, QLatin1StringView("scale"));

    return true;
}

int main(int argc, char **argv)
{
    qputenv("QSG_INFO", "1");
    qputenv("QSG_RHI_PROFILE", "1");

    QApplication app(argc, argv);
    r.init();

    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    QCommandLineOption glOption({ "g", "opengl" }, QLatin1String("OpenGL"));
    cmdLineParser.addOption(glOption);
    QCommandLineOption vkOption({ "v", "vulkan" }, QLatin1String("Vulkan"));
    cmdLineParser.addOption(vkOption);
    QCommandLineOption d3d11Option({ "d", "d3d11" }, QLatin1String("Direct3D 11"));
    cmdLineParser.addOption(d3d11Option);
    QCommandLineOption d3d12Option({ "D", "d3d12" }, QLatin1String("Direct3D 12"));
    cmdLineParser.addOption(d3d12Option);
    QCommandLineOption mtlOption({ "m", "metal" }, QLatin1String("Metal"));
    cmdLineParser.addOption(mtlOption);

    cmdLineParser.process(app);

    InputScene scene;
    if (!parseScene(&scene))
        return 1;

    QElapsedTimer timer;
    timer.start();
    for (const InputScene::Model &inputModel : scene.models) {
        Model model;
        if (model.load(inputModel.source, inputModel.key))
            r.models.append(model);
        else
            return 1;
    }
    for (const InputScene::ModelInstance &inputInstance : scene.modelInstances) {
        ModelInstance modelInstance;
        for (qsizetype modelIndex = 0; modelIndex < r.models.count(); ++modelIndex) {
            if (inputInstance.key == r.models[modelIndex].key) {
                modelInstance.modelIndex = modelIndex;
                modelInstance.worldTransform.position = inputInstance.transform.position;
                modelInstance.worldTransform.scale = inputInstance.transform.scale;
                modelInstance.worldTransform.eulerRotation = inputInstance.transform.rotation;
                modelInstance.worldTransform.updateRotation();
                r.modelInstances.append(modelInstance);
                for (const Model::Mesh &mesh : std::as_const(r.models[modelIndex].meshes))
                    r.totalSubMeshCount += mesh.subMeshes.count();
                break;
            }
        }
    }
    qDebug() << r.models.count() << "models loaded in" << timer.elapsed() << "ms,"
             << r.totalSubMeshCount << "submeshes in total,"
             << r.modelInstances.count() << "model instances";

    r.camera.worldTransform.position = scene.camera.position;
    r.camera.worldTransform.eulerRotation = scene.camera.rotation;
    r.camera.worldTransform.updateRotation();

    ExampleRhiWidget rhiWidget;

    if (cmdLineParser.isSet(glOption))
        rhiWidget.setApi(QRhiWidget::Api::OpenGL);
    if (cmdLineParser.isSet(vkOption))
        rhiWidget.setApi(QRhiWidget::Api::Vulkan);
    if (cmdLineParser.isSet(d3d11Option))
        rhiWidget.setApi(QRhiWidget::Api::D3D11);
    if (cmdLineParser.isSet(d3d12Option))
        rhiWidget.setApi(QRhiWidget::Api::D3D12);
    if (cmdLineParser.isSet(mtlOption))
        rhiWidget.setApi(QRhiWidget::Api::Metal);

    rhiWidget.resize(1280, 720);
    rhiWidget.show();

    qDebug("Press ` or F9 to toggle GUI, WASDRF and mouse to control the camera");

    return app.exec();
}
