#include <QGuiApplication>
#include <QOffscreenSurface>
#include <QCommandLineParser>
#include <QFileInfo>
#include <QElapsedTimer>
#include <QImage>
#include <thread>
#include <mutex>
#include <rhi/qrhi.h>

#define CGLTF_IMPLEMENTATION
#include "../3rdparty/cgltf/cgltf.h"

static double totalMs = 0;

static void printTimeAndRestart(const char *msg, QElapsedTimer *timer)
{
    const qint64 ns = timer->nsecsElapsed();
    const double ms = ns/double(1000000);
    totalMs += ms;
    qDebug().nospace() << msg << " " << ns << " nsecs (" << ms << " ms)";
    timer->restart();
}

int main(int argc, char **argv)
{
    QGuiApplication app(argc, argv);

    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    cmdLineParser.addPositionalArgument(QLatin1String("file"),
                                        QObject::tr("glTF file (.glb or .gltf) to parse for texture maps"),
                                        QObject::tr("file"));

    QCommandLineOption mipOption({ "m", "mipmaps" }, QObject::tr("Generate mipmaps."));
    cmdLineParser.addOption(mipOption);

    QCommandLineOption threadOption({ "t", "thread" }, QObject::tr("Load QImages concurrently."));
    cmdLineParser.addOption(threadOption);
    QCommandLineOption threadCountOption({ "c", "count" }, QObject::tr("Override thread count"), QObject::tr("count"));
    cmdLineParser.addOption(threadCountOption);

    QCommandLineOption glOption({ "g", "opengl" }, QLatin1String("OpenGL"));
    cmdLineParser.addOption(glOption);
    QCommandLineOption vkOption({ "v", "vulkan" }, QLatin1String("Vulkan"));
    cmdLineParser.addOption(vkOption);
    QCommandLineOption d3d11Option({ "d", "d3d11" }, QLatin1String("Direct3D 11"));
    cmdLineParser.addOption(d3d11Option);
    QCommandLineOption d3d12Option({ "D", "d3d12" }, QLatin1String("Direct3D 12"));
    cmdLineParser.addOption(d3d12Option);
    QCommandLineOption mtlOption({ "M", "metal" }, QLatin1String("Metal"));
    cmdLineParser.addOption(mtlOption);

    cmdLineParser.process(app);

    if (cmdLineParser.positionalArguments().isEmpty()) {
        cmdLineParser.showHelp();
        return 0;
    }

    const QString filenameQStr = cmdLineParser.positionalArguments().first();
    const QByteArray filename = filenameQStr.toUtf8();

    cgltf_options options = {};
    cgltf_data *data = nullptr;
    QElapsedTimer timer;
    timer.start();
    cgltf_result result = cgltf_parse_file(&options, filename.constData(), &data);
    if (result != cgltf_result_success) {
        qWarning("Failed to parse glTF file: %d", int(result));
        return 0;
    }
    printTimeAndRestart("cgltf_parse_file took", &timer);

    result = cgltf_load_buffers(&options, data, filename.constData());
    if (result != cgltf_result_success) {
        qWarning("Failed to load buffers: %d", int(result));
        cgltf_free(data);
        return 0;
    }
    printTimeAndRestart("cgltf_load_buffers took", &timer);

	for (cgltf_size i = 0; i < data->buffers_count; ++i) {
        const cgltf_buffer *buffer = &data->buffers[i];
        qDebug() << "Buffer" << i << "has" << buffer->size << "bytes";
    }

	for (cgltf_size i = 0; i < data->meshes_count; ++i) {
        const cgltf_mesh *mesh = &data->meshes[i];
        qDebug() << "Mesh" << i << "has" << mesh->primitives_count << "submeshes";
    }

    const QString prefix = QFileInfo(filenameQStr).absolutePath() + QLatin1Char('/');
    QStringList imagePathList;
    for (cgltf_size i = 0; i < data->images_count; ++i) {
        const cgltf_image *image = &data->images[i];
        const QString fn = prefix + QString::fromUtf8(image->uri);
        imagePathList.append(fn);
    }

    QVector<QImage> images;
    std::mutex mutex;
    static auto loadFunc = [&images, &mutex](const QString &fn) {
        QImage img;
        img.load(fn);
        if (img.isNull()) {
            qWarning() << "Failed to load" << fn;
        } else {
            {
                std::lock_guard<std::mutex> guard(mutex);
                images.append(img);
            }
            qDebug() << fn << img;
        }
    };

    int threadCount = std::thread::hardware_concurrency();
    if (cmdLineParser.isSet(threadCountOption))
        threadCount = qMax(1, cmdLineParser.value(threadCountOption).toInt());

    timer.restart();
    if (cmdLineParser.isSet(threadOption)) {
        qDebug() << "Loading" << imagePathList.count() << "images on" << threadCount << "threads in parallel";
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
        printTimeAndRestart("Loading concurrently via QImage took", &timer);
    } else {
        qDebug() << "Loading" << imagePathList.count() << "images sequentially";
        for (const QString &fn : imagePathList)
            loadFunc(fn);
        printTimeAndRestart("Loading sequentially via QImage took", &timer);
    }

    cgltf_free(data);

    QRhi::Implementation graphicsApi;
#if defined(Q_OS_WIN)
    graphicsApi = QRhi::D3D11;
#elif defined(Q_OS_MACOS) || defined(Q_OS_IOS)
    graphicsApi = QRhi::Metal;
#elif QT_CONFIG(vulkan)
    graphicsApi = QRhi::Vulkan;
#else
    graphicsApi = QRhi::OpenGLES2;
#endif
    if (cmdLineParser.isSet(glOption))
        graphicsApi = QRhi::OpenGLES2;
    if (cmdLineParser.isSet(vkOption))
        graphicsApi = QRhi::Vulkan;
    if (cmdLineParser.isSet(d3d11Option))
        graphicsApi = QRhi::D3D11;
    if (cmdLineParser.isSet(d3d12Option))
        graphicsApi = QRhi::D3D12;
    if (cmdLineParser.isSet(mtlOption))
        graphicsApi = QRhi::Metal;

#if QT_CONFIG(vulkan)
    QVulkanInstance inst;
#endif
    std::unique_ptr<QRhi> rhi;
    std::unique_ptr<QOffscreenSurface> fallbackSurface;
#if defined(Q_OS_WIN)
    if (graphicsApi == QRhi::D3D11) {
        QRhiD3D11InitParams params;
        rhi.reset(QRhi::create(QRhi::D3D11, &params));
    } else if (graphicsApi == QRhi::D3D12) {
        QRhiD3D12InitParams params;
        rhi.reset(QRhi::create(QRhi::D3D12, &params));
    }
#endif
#if defined(Q_OS_MACOS) || defined(Q_OS_IOS)
    if (graphicsApi == QRhi::Metal) {
        QRhiMetalInitParams params;
        rhi.reset(QRhi::create(QRhi::Metal, &params));
    }
#endif
#if QT_CONFIG(vulkan)
    if (graphicsApi == QRhi::Vulkan) {
        inst.setExtensions(QRhiVulkanInitParams::preferredInstanceExtensions());
        if (inst.create()) {
            QRhiVulkanInitParams params;
            params.inst = &inst;
            rhi.reset(QRhi::create(QRhi::Vulkan, &params));
        }
    }
#endif
    if (!rhi) {
        fallbackSurface.reset(QRhiGles2InitParams::newFallbackSurface());
        QRhiGles2InitParams params;
        params.fallbackSurface = fallbackSurface.get();
        rhi.reset(QRhi::create(QRhi::OpenGLES2, &params));
    }

    if (rhi) {
        qDebug() << rhi->backendName() << rhi->driverInfo();
    } else {
        qWarning("Failed to initialize RHI");
        return 0;
    }

    printTimeAndRestart("QRhi init took", &timer);

    const bool mips = cmdLineParser.isSet(mipOption);
    QVector<QRhiTexture *> textures;
    for (const QImage &image : images) {
        QRhiTexture::Flags flags;
        if (mips)
            flags |= QRhiTexture::MipMapped | QRhiTexture::UsedWithGenerateMips;
        QRhiTexture *tex = rhi->newTexture(QRhiTexture::RGBA8, image.size(), 1, flags);
        tex->create();
        textures.append(tex);
    }
    printTimeAndRestart("QRhiTexture creation took", &timer);

    qDebug("Converting QImages to RGBA8888 (if not already that)");
    for (qsizetype i = 0; i < images.count(); ++i)
        images[i] = images[i].convertToFormat(QImage::Format_RGBA8888);
    printTimeAndRestart("Format conversion took", &timer);

    qDebug() << "Issuing uploadTexture() for" << textures.count() << "textures";
    if (mips)
        qDebug() << "Also generating mipmaps";
    QRhiCommandBuffer *cb;
    rhi->beginOffscreenFrame(&cb);
    QRhiResourceUpdateBatch *u = rhi->nextResourceUpdateBatch();
    for (qsizetype i = 0; i < images.count(); ++i) {
        u->uploadTexture(textures[i], images[i]);
        if (mips)
            u->generateMips(textures[i]);
    }
    cb->resourceUpdate(u);
    rhi->endOffscreenFrame();
    printTimeAndRestart("Image data upload took", &timer);

    qDeleteAll(textures);

    qDebug() << totalMs << "ms in total";
    return 0;
}
