#include <QGuiApplication>
#include <QCommandLineParser>
#include <QFileInfo>
#include <QElapsedTimer>
#include <QImage>
#include <thread>

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
    QCommandLineOption threadOption({ "t", "thread" }, QObject::tr("Load QImages concurrently."));
    cmdLineParser.addOption(threadOption);
    QCommandLineOption threadCountOption({ "c", "count" }, QObject::tr("Override thread count"), QObject::tr("count"));
    cmdLineParser.addOption(threadCountOption);

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
    static auto loadFunc = [&images](const QString &fn) {
        QImage img;
        img.load(fn);
        if (img.isNull()) {
            qWarning() << "Failed to load" << fn;
        } else {
            images.append(img);
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
        }
        printTimeAndRestart("Loading concurrently via QImage took", &timer);
    } else {
        qDebug() << "Loading" << imagePathList.count() << "images sequentially";
        for (const QString &fn : imagePathList)
            loadFunc(fn);
        printTimeAndRestart("Loading sequentially via QImage took", &timer);
    }

    cgltf_free(data);

    qDebug() << totalMs << "ms in total";
    return 0;
}
