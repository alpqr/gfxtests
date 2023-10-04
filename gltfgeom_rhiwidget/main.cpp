// Copyright (C) 2023 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause

#include <QApplication>
#include <QCommandLineParser>
#include <QElapsedTimer>
#include <QRhiWidget>
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

private:
    Gltf m_gltf;
    QRhi *m_rhi = nullptr;
    std::unique_ptr<QRhiBuffer> m_vbuf;
    std::unique_ptr<QRhiBuffer> m_ibuf;
    std::unique_ptr<QRhiBuffer> m_ubuf;
    std::unique_ptr<QRhiShaderResourceBindings> m_srb;
    std::unique_ptr<QRhiGraphicsPipeline> m_pipeline;
    QMatrix4x4 m_viewProjection;
    float m_rotation = 0.0f;
};

ExampleRhiWidget::ExampleRhiWidget(const QString &gltfFilename, QWidget *parent)
    : QRhiWidget(parent)
{
    QElapsedTimer timer;
    timer.start();
    m_gltf.load(gltfFilename);
    if (m_gltf.isValid())
        qDebug() << "glTF asset loaded in" << timer.elapsed() << "ms";
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
    m_viewProjection = m_rhi->clipSpaceCorrMatrix();
    m_viewProjection.perspective(45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 1000.0f);
    m_viewProjection.translate(0, -100, -600);
    //m_viewProjection.translate(0, 0, -10);
}

void ExampleRhiWidget::render(QRhiCommandBuffer *cb)
{
    QRhiResourceUpdateBatch *resourceUpdates = m_rhi->nextResourceUpdateBatch();
    m_rotation += 0.5f;
    QMatrix4x4 modelViewProjection = m_viewProjection;
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

int main(int argc, char **argv)
{
    QApplication app(argc, argv);

    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    cmdLineParser.addPositionalArgument(QLatin1String("file"),
                                        QObject::tr("glTF file (.glb or .gltf)"),
                                        QObject::tr("file"));

    cmdLineParser.process(app);

    if (cmdLineParser.positionalArguments().isEmpty()) {
        cmdLineParser.showHelp();
        return 0;
    }

    ExampleRhiWidget rhiWidget(cmdLineParser.positionalArguments().first());
    rhiWidget.setApi(QRhiWidget::Api::D3D12);
    rhiWidget.setDebugLayer(true);
    rhiWidget.resize(1280, 720);
    rhiWidget.show();
    return app.exec();
}
