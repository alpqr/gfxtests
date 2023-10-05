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

#define CGLTF_IMPLEMENTATION
#include "../3rdparty/cgltf/cgltf.h"

struct Gltf
{
    ~Gltf();
    void reset();
    bool isValid() const { return data != nullptr; }
    bool load(const QString &filename);

    struct SubMeshInfo {
        quint32 indexByteOffset = 0;
        quint32 indexCount = 0;
        quint32 positionsByteOffset = 0;
    };
    struct MeshInfo {
        QVector<SubMeshInfo> subMeshInfo;
    };

    cgltf_data *data = nullptr;
    QVector<QImage> images;
    QByteArray indices;
    QByteArray positions;
    QVector<MeshInfo> meshInfo;
};

Gltf::~Gltf()
{
    reset();
}

void Gltf::reset()
{
    images.clear();
    indices.clear();
    positions.clear();
    meshInfo.clear();
    cgltf_free(data);
    data = nullptr;
}

bool Gltf::load(const QString &filename)
{
    reset();

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

    const QString prefix = QFileInfo(filename).absolutePath() + QLatin1Char('/');
    QStringList imagePathList;
    for (cgltf_size i = 0; i < data->images_count; ++i) {
        const cgltf_image *image = &data->images[i];
        const QString fn = prefix + QString::fromUtf8(image->uri);
        imagePathList.append(fn);
    }

    static auto loadFunc = [this](const QString &fn) {
        QImage img;
        img.load(fn);
        if (img.isNull())
            qWarning() << "Failed to load" << fn;
        images.append(img);
    };

    const int threadCount = std::thread::hardware_concurrency();
    for (int t = 0; t < imagePathList.count(); t += threadCount) {
        const int n = std::min<int>(imagePathList.count() - t, threadCount);
        QVarLengthArray<std::thread *, 16> threadList;
        for (int i = 0; i < n; ++i) {
            const QString fn = imagePathList[t + i];
            threadList.append(new std::thread(std::bind(loadFunc, fn)));
        }
        for (std::thread *thread : threadList)
            thread->join();
        qDeleteAll(threadList);
    }

    cgltf_size totalIndexCount = 0;
    cgltf_size totalAccessorCount = 0;
    for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        for (cgltf_size j = 0; j < data->meshes[i].primitives_count; ++j) {
            totalIndexCount += data->meshes[i].primitives[j].indices->count;
            for (cgltf_size a = 0; a < data->meshes[i].primitives[j].attributes_count; ++a)
                totalAccessorCount += data->meshes[i].primitives[j].attributes[a].data->count;
        }
    }
    indices.resize(totalIndexCount * sizeof(quint32));
    size_t currentIndexByteOffset = 0;
    positions.resize(totalAccessorCount * 3 * sizeof(float));
    size_t currentPositionsByteOffset = 0;

    meshInfo.resize(data->meshes_count);
    for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        const cgltf_mesh *mesh = &data->meshes[i];
        meshInfo[i].subMeshInfo.reserve(mesh->primitives_count);

        for (cgltf_size j = 0; j < mesh->primitives_count; ++j) {
            const cgltf_primitive *submesh = &mesh->primitives[j];
            SubMeshInfo subMeshInfo;
            subMeshInfo.positionsByteOffset = currentPositionsByteOffset;
            for (cgltf_size a = 0; a < submesh->attributes_count; ++a) {
                const cgltf_attribute *attrib = &submesh->attributes[a];
                const cgltf_accessor *accessor = attrib->data;
                const cgltf_buffer_view *view = accessor->buffer_view;
                cgltf_size offset = accessor->offset + view->offset;
                for (cgltf_size acc = 0; acc < accessor->count; ++acc) {
                    const quint8 *addr = static_cast<const quint8 *>(view->buffer->data) + offset;
                    if (attrib->type == cgltf_attribute_type_position) {
                        if (accessor->type == cgltf_type_vec3) {
                            const float *p = reinterpret_cast<const float *>(addr);
                            float *dst = reinterpret_cast<float *>(positions.data() + currentPositionsByteOffset);
                            *dst++ = p[0];
                            *dst++ = p[1];
                            *dst++ = p[2];
                            currentPositionsByteOffset += 3 * sizeof(float);
                        }
                    }
                    if (view->stride)
                        offset += view->stride;
                    else
                        offset += accessor->stride;
                }
            }

            const cgltf_accessor *indexAccessor = submesh->indices;
            subMeshInfo.indexByteOffset = currentIndexByteOffset;
            subMeshInfo.indexCount = indexAccessor->count;
            const quint8 *addr = static_cast<const quint8 *>(indexAccessor->buffer_view->buffer->data) + indexAccessor->buffer_view->offset + indexAccessor->offset;
            quint32 *dst = reinterpret_cast<quint32 *>(indices.data() + currentIndexByteOffset);
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

            meshInfo[i].subMeshInfo.append(subMeshInfo);
        }
    }

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
    ExampleRhiWidget(const QString &gltfFilename, QWidget *parent = nullptr);
    void initialize(QRhiCommandBuffer *cb) override;
    void render(QRhiCommandBuffer *cb) override;
    void keyPressEvent(QKeyEvent *event) override;
    void keyReleaseEvent(QKeyEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;

private:
    Gltf m_gltf;
    Camera m_camera;
    WASDController m_wasd;
    QRhi *m_rhi = nullptr;
    std::unique_ptr<QRhiBuffer> m_vbuf;
    std::unique_ptr<QRhiBuffer> m_ibuf;
    std::unique_ptr<QRhiBuffer> m_ubuf;
    std::unique_ptr<QRhiShaderResourceBindings> m_srb;
    std::unique_ptr<QRhiGraphicsPipeline> m_pipeline;
    float m_rotation = 0.0f;
};

ExampleRhiWidget::ExampleRhiWidget(const QString &gltfFilename, QWidget *parent)
    : QRhiWidget(parent),
      m_wasd(&m_camera)
{
    QElapsedTimer timer;
    timer.start();

    m_gltf.load(gltfFilename);
    if (m_gltf.isValid())
        qDebug() << "glTF asset loaded in" << timer.elapsed() << "ms";

    m_camera.position = QVector3D(0, -100, -600);
}

void ExampleRhiWidget::initialize(QRhiCommandBuffer *cb)
{
    m_rhi = rhi();
    if (!m_pipeline) {
        m_vbuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::VertexBuffer, m_gltf.positions.size()));
        m_vbuf->create();

        m_ibuf.reset(m_rhi->newBuffer(QRhiBuffer::Immutable, QRhiBuffer::IndexBuffer, m_gltf.indices.size()));
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
            { 3 * sizeof(float) }
        });
        inputLayout.setAttributes({
            { 0, 0, QRhiVertexInputAttribute::Float3, 0 }
        });
        m_pipeline->setVertexInputLayout(inputLayout);
        m_pipeline->setShaderResourceBindings(m_srb.get());
        m_pipeline->setRenderPassDescriptor(renderTarget()->renderPassDescriptor());
        m_pipeline->create();

        QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();
        resourceUpdates->uploadStaticBuffer(m_vbuf.get(), m_gltf.positions.constData());
        resourceUpdates->uploadStaticBuffer(m_ibuf.get(), m_gltf.indices.constData());
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
    m_rotation += 0.2f;
    modelViewProjection.rotate(m_rotation, 0, 1, 0);
    resourceUpdates->updateDynamicBuffer(m_ubuf.get(), 0, 64, modelViewProjection.constData());

    const QColor clearColor = QColor::fromRgbF(0.4f, 0.7f, 0.0f, 1.0f);
    cb->beginPass(renderTarget(), clearColor, { 1.0f, 0 }, resourceUpdates);

    cb->setGraphicsPipeline(m_pipeline.get());
    const QSize outputSize = colorTexture()->pixelSize();
    cb->setViewport(QRhiViewport(0, 0, outputSize.width(), outputSize.height()));
    cb->setShaderResources();
    for (const Gltf::MeshInfo &meshInfo : std::as_const(m_gltf.meshInfo)) {
        for (const Gltf::SubMeshInfo &subMeshInfo : std::as_const(meshInfo.subMeshInfo)) {
            const QRhiCommandBuffer::VertexInput vbufBinding(m_vbuf.get(), subMeshInfo.positionsByteOffset);
            cb->setVertexInput(0, 1, &vbufBinding, m_ibuf.get(), subMeshInfo.indexByteOffset, QRhiCommandBuffer::IndexUInt32);
            cb->drawIndexed(subMeshInfo.indexCount);
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

int main(int argc, char **argv)
{
    QApplication app(argc, argv);

    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    cmdLineParser.addPositionalArgument(QLatin1String("file"),
                                        QObject::tr("glTF file (.glb or .gltf)"),
                                        QObject::tr("file"));
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

    if (cmdLineParser.positionalArguments().isEmpty()) {
        cmdLineParser.showHelp();
        return 0;
    }

    const QStringList fileArgs = cmdLineParser.positionalArguments();
    ExampleRhiWidget rhiWidget(fileArgs.first());

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
