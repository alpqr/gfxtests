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

struct Model
{
    struct SubMeshInfo {
        quint32 indexByteOffset = 0;
        quint32 indexCount = 0;
        quint32 vertexByteOffset = 0;
        std::optional<QVector3D> minPos;
        std::optional<QVector3D> maxPos;
        std::optional<qsizetype> materialIndex;
    };

    struct MeshInfo {
        QVector<SubMeshInfo> subMeshInfo;
    };

    bool load(const QString &filename);

    QVector<MeshInfo> meshInfo;
    QString sourceFilename;
};

struct R
{
    struct Image {
        QImage image;
        QString sourceFilename;
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
    quint32 vertexByteStride;
    QByteArray indices;
    QByteArray vertices;
    QVector<Image> images;
    QVector<Material> materials;

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

void R::reset()
{
    indices.clear();
    vertices.clear();
    images.clear();
    for (Material &mat : materials) {
        if (mat.baseColorMap.has_value())
            delete mat.baseColorMap->texture;
    }
    materials.clear();
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
    cb->resourceUpdate(u);
    qDebug() << "Texture uploads" << timer.elapsed() << "ms";
}

bool Model::load(const QString &filename)
{
    meshInfo.clear();
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

    const QString prefix = QFileInfo(filename).absolutePath() + QLatin1Char('/');
    QStringList imagePathList;
    for (cgltf_size i = 0; i < data->images_count; ++i) {
        const cgltf_image *image = &data->images[i];
        const QString fn = prefix + QString::fromUtf8(image->uri);
        imagePathList.append(fn);
    }

    std::mutex mutex;
    static auto loadImageFunc = [this, &mutex](const QString &fn) {
        QImage img;
        img.load(fn);
        if (img.isNull()) {
            qWarning() << "Failed to load" << fn;
            img = QImage(16, 16, QImage::Format_RGBA8888);
            img.fill(Qt::magenta);
        }
        {
            std::lock_guard<std::mutex> guard(mutex);
            r.images.append({ img, fn });
        }
    };

    const int threadCount = std::thread::hardware_concurrency();
    const int firstImageGlobalIndex = r.images.count();
    for (int t = 0; t < imagePathList.count(); t += threadCount) {
        const int n = std::min<int>(imagePathList.count() - t, threadCount);
        QVarLengthArray<std::thread *, 16> threadList;
        for (int i = 0; i < n; ++i) {
            const QString fn = imagePathList[t + i];
            threadList.append(new std::thread(std::bind(loadImageFunc, fn)));
        }
        for (std::thread *thread : threadList)
            thread->join();
        qDeleteAll(threadList);
    }

    const int firstMaterialGlobalIndex = r.materials.count();
    for (cgltf_size i = 0; i < data->materials_count; ++i) {
        const cgltf_material *gmat = &data->materials[i];
        R::Material mat;
        if (gmat->has_pbr_metallic_roughness) {
            const cgltf_texture_view *baseColorView = &gmat->pbr_metallic_roughness.base_color_texture;
            if (baseColorView->texture) {
                R::TextureMap t;
                t.imageIndex = firstImageGlobalIndex + cgltf_image_index(data, baseColorView->texture->image);
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

    meshInfo.resize(data->meshes_count);
    for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        const cgltf_mesh *mesh = &data->meshes[i];
        meshInfo[i].subMeshInfo.reserve(mesh->primitives_count);

        for (cgltf_size j = 0; j < mesh->primitives_count; ++j) {
            const cgltf_primitive *submesh = &mesh->primitives[j];
            SubMeshInfo subMeshInfo;
            const qsizetype subMeshVertexIndex = positions.count();

            for (cgltf_size a = 0; a < submesh->attributes_count; ++a) {
                const cgltf_attribute *attrib = &submesh->attributes[a];
                const cgltf_accessor *accessor = attrib->data;
                const cgltf_buffer_view *view = accessor->buffer_view;
                const cgltf_size stride = view->stride ? view->stride : accessor->stride;
                const quint8 *src = static_cast<const quint8 *>(view->buffer->data) + accessor->offset + view->offset;

                if (attrib->type == cgltf_attribute_type_position) {
                    if (accessor->has_min)
                        subMeshInfo.minPos = QVector3D(accessor->min[0], accessor->min[1], accessor->min[2]);
                    if (accessor->has_max)
                        subMeshInfo.maxPos = QVector3D(accessor->max[0], accessor->max[1], accessor->max[2]);
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

            subMeshInfo.vertexByteOffset = subMeshVertexIndex * r.vertexByteStride;

            const cgltf_accessor *indexAccessor = submesh->indices;
            subMeshInfo.indexByteOffset = currentIndexByteOffset;
            subMeshInfo.indexCount = indexAccessor->count;
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

            if (submesh->material)
                subMeshInfo.materialIndex = firstMaterialGlobalIndex + cgltf_material_index(data, submesh->material);

            meshInfo[i].subMeshInfo.append(subMeshInfo);
        }
    }

    const quint32 dstOffset = r.vertexStorage(positions.count() * r.vertexByteStride);
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

struct Camera
{
    QVector3D position;
    QVector3D scale = { 1, 1, 1 };
    QVector3D pivot;
    QVector3D eulerRotation;

    void updateProjection(const QMatrix4x4 &clipSpaceCorrMatrix, float fovDegrees, float aspectRatio,
                          float clipNear, float clipFar);
    void updateWorldTransform();
    void updateViewProjection();
    void updateRotation();
    QVector3D mapDirection(const QVector3D &localDirection) const;
    QVector3D forward() const { return mapDirection(QVector3D(0, 0, -1)).normalized(); }
    QVector3D up() const { return mapDirection(QVector3D(0, 1, 0)).normalized(); }
    QVector3D right() const { return mapDirection(QVector3D(1, 0, 0)).normalized(); }

    QMatrix4x4 projection;
    QMatrix4x4 worldTransform;
    QMatrix4x4 viewProjection;
    QQuaternion rotation;
};

static inline QVector3D transformMat33(const QMatrix3x3 &m, const QVector3D &v)
{
    const QVector3D c0 = QVector3D(m(0, 0), m(1, 0), m(2, 0));
    const QVector3D c1 = QVector3D(m(0, 1), m(1, 1), m(2, 1));
    const QVector3D c2 = QVector3D(m(0, 2), m(1, 2), m(2, 2));
    return c0 * v.x() + c1 * v.y() + c2 * v.z();
}

QVector3D Camera::mapDirection(const QVector3D &localDirection) const
{
    return transformMat33(worldTransform.normalMatrix(), localDirection);
}

void Camera::updateProjection(const QMatrix4x4 &clipSpaceCorrMatrix, float fovDegrees, float aspectRatio,
                              float clipNear, float clipFar)
{
    projection = clipSpaceCorrMatrix;
    projection.perspective(fovDegrees, aspectRatio, clipNear, clipFar);
}

void Camera::updateWorldTransform()
{
    worldTransform.setToIdentity();

    const QVector3D offset = (-pivot * scale);

    worldTransform(0, 0) = scale[0];
    worldTransform(1, 1) = scale[1];
    worldTransform(2, 2) = scale[2];

    worldTransform(0, 3) = offset[0];
    worldTransform(1, 3) = offset[1];
    worldTransform(2, 3) = offset[2];

    worldTransform = QMatrix4x4(rotation.toRotationMatrix()) * worldTransform;

    worldTransform(0, 3) += position[0];
    worldTransform(1, 3) += position[1];
    worldTransform(2, 3) += position[2];
}

void Camera::updateViewProjection()
{
    QMatrix4x4 m(Qt::Uninitialized);
    m.setColumn(0, worldTransform.column(0).normalized());
    m.setColumn(1, worldTransform.column(1).normalized());
    m.setColumn(2, worldTransform.column(2).normalized());
    m.setColumn(3, worldTransform.column(3));
    viewProjection = projection * m.inverted();
}

void Camera::updateRotation()
{
    rotation = QQuaternion::fromEulerAngles(eulerRotation).normalized();
}

struct WASDController
{
    WASDController(Camera *camera) : camera(camera) { }
    void update();

    float moveSpeed = 5.0f;
    float xLookSpeed = 0.1f;
    float yLookSpeed = 0.1f;

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
        camera->updateWorldTransform();
        const float speed = moveSpeed * delta;
        QVector3D direction;
        if (state.moveForward || state.moveBack) {
            if (state.moveForward)
                direction = camera->forward();
            else
                direction = -camera->forward();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->position += velocity;
        }
        if (state.moveRight || state.moveLeft) {
            if (state.moveRight)
                direction = camera->right();
            else
                direction = -camera->right();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->position += velocity;
        }
        if (state.moveUp || state.moveDown) {
            if (state.moveUp)
                direction = camera->up();
            else
                direction = -camera->up();
            const QVector3D velocity(direction.x() * speed, direction.y() * speed, direction.z() * speed);
            camera->position += velocity;
        }
    }

    if (state.moveMouse) {
        QVector3D r = camera->eulerRotation;
        float rx = (state.lastMousePos.x() - state.currentMousePos.x()) * xLookSpeed * delta;
        r.setY(r.y() + rx);

        float ry = -1.0f * ((state.lastMousePos.y() - state.currentMousePos.y()) * -yLookSpeed * delta);
        r.setX(r.x() + ry);

        camera->eulerRotation = r;
        camera->updateRotation();

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

private:
    Camera m_camera;
    WASDController m_wasd;
    QRhi *m_rhi = nullptr;
    std::unique_ptr<QRhiBuffer> m_vbuf;
    std::unique_ptr<QRhiBuffer> m_ibuf;
    std::unique_ptr<QRhiBuffer> m_ubuf;
    std::unique_ptr<QRhiShaderResourceBindings> m_srb;
    std::unique_ptr<QRhiGraphicsPipeline> m_pipeline;
};

ExampleRhiWidget::ExampleRhiWidget(QWidget *parent)
    : QRhiWidget(parent),
      m_wasd(&m_camera)
{
    m_camera.position = QVector3D(0, 0, -600);
}

void ExampleRhiWidget::initialize(QRhiCommandBuffer *cb)
{
    if (!r.rhi) {
        m_rhi = rhi();
        r.rhi = m_rhi;
        r.rhi->addCleanupCallback([](QRhi*) { r.reset(); });
        r.createTextures(cb);
    }

    if (!m_pipeline) {
        m_vbuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::VertexBuffer, r.vertices.size()));
        m_vbuf->create();

        m_ibuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::IndexBuffer, r.indices.size()));
        m_ibuf->create();

        m_ubuf.reset(m_rhi->newBuffer(QRhiBuffer::Dynamic, QRhiBuffer::UniformBuffer, 64));
        m_ubuf->create();

        m_srb.reset(m_rhi->newShaderResourceBindings());
        m_srb->setBindings({
            QRhiShaderResourceBinding::uniformBuffer(0, QRhiShaderResourceBinding::VertexStage, m_ubuf.get()),
        });
        m_srb->create();

        m_pipeline.reset(m_rhi->newGraphicsPipeline());
        m_pipeline->setShaderStages({
            { QRhiShaderStage::Vertex, getShader(QLatin1String(":/shaders/basic.vert.qsb")) },
            { QRhiShaderStage::Fragment, getShader(QLatin1String(":/shaders/basic.frag.qsb")) }
        });
        m_pipeline->setDepthTest(true);
        m_pipeline->setDepthWrite(true);
        m_pipeline->setPolygonMode(QRhiGraphicsPipeline::Line);
        QRhiVertexInputLayout inputLayout;
        inputLayout.setBindings({
            { 8 * sizeof(float) }
        });
        inputLayout.setAttributes({
            { 0, 0, QRhiVertexInputAttribute::Float3, 0 },
            { 0, 1, QRhiVertexInputAttribute::Float2, 3 * sizeof(float) },
            { 0, 2, QRhiVertexInputAttribute::Float3, 5 * sizeof(float) }
        });
        m_pipeline->setVertexInputLayout(inputLayout);
        m_pipeline->setShaderResourceBindings(m_srb.get());
        m_pipeline->setRenderPassDescriptor(renderTarget()->renderPassDescriptor());
        m_pipeline->create();

        QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();
        resourceUpdates->uploadStaticBuffer(m_vbuf.get(), r.vertices.constData());
        resourceUpdates->uploadStaticBuffer(m_ibuf.get(), r.indices.constData());
        cb->resourceUpdate(resourceUpdates);
    }

    const QSize outputSize = colorTexture()->pixelSize();
    m_camera.updateProjection(m_rhi->clipSpaceCorrMatrix(), 45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 10000.0f);
}

void ExampleRhiWidget::render(QRhiCommandBuffer *cb)
{
    QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();

    m_wasd.update();
    m_camera.updateWorldTransform();
    m_camera.updateViewProjection();

    QMatrix4x4 modelViewProjection = m_camera.viewProjection;
    //modelViewProjection.rotate(m_rotation, 0, 1, 0);
    resourceUpdates->updateDynamicBuffer(m_ubuf.get(), 0, 64, modelViewProjection.constData());

    const QColor clearColor = QColor::fromRgbF(0.4f, 0.7f, 0.0f, 1.0f);
    cb->beginPass(renderTarget(), clearColor, { 1.0f, 0 }, resourceUpdates);

    cb->setGraphicsPipeline(m_pipeline.get());
    const QSize outputSize = colorTexture()->pixelSize();
    cb->setViewport(QRhiViewport(0, 0, outputSize.width(), outputSize.height()));
    cb->setShaderResources();
    for (const Model &model : std::as_const(r.models)) {
        for (const Model::MeshInfo &meshInfo : model.meshInfo) {
            for (const Model::SubMeshInfo &subMeshInfo : meshInfo.subMeshInfo) {
                const QRhiCommandBuffer::VertexInput vbufBinding(m_vbuf.get(), subMeshInfo.vertexByteOffset);
                cb->setVertexInput(0, 1, &vbufBinding, m_ibuf.get(), subMeshInfo.indexByteOffset, QRhiCommandBuffer::IndexUInt32);
                cb->drawIndexed(subMeshInfo.indexCount);
            }
        }
    }

    cb->endPass();

    update();
}

void ExampleRhiWidget::keyPressEvent(QKeyEvent *event)
{
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

struct InputScene
{
    struct Model {
        QString key;
        QString source;
    };
    QVector<Model> models;
};

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
    for (qsizetype i = 0; i < models.count(); ++i) {
        const QJsonObject model = models[i].toObject();
        InputScene::Model m;
        m.key = model["key"].toString();
        m.source = model["source"].toString();
        if (!m.key.isEmpty() && !m.source.isEmpty())
            s->models.append(m);
    }
    return true;
}

int main(int argc, char **argv)
{
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
        if (!model.load(inputModel.source))
            return 1;
        r.models.append(model);
    }
    qDebug() << scene.models.count() << "models loaded in" << timer.elapsed() << "ms";

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

    return app.exec();
}
