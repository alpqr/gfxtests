Qt graphics tests, experiments, and benchmarks.

The test apps requires Qt 6.7 (the 'dev' branch as of 10/2023) generally.

- qimage_test

    Loads all texture maps for a glTF asset via QImage (assuming QImage-supported
    formats such as jpg or png) and measures the elapsed time.

    Run e.g. like this: qimage_test_rhi ..\3rdparty\sponza\Sponza.gltf

    Add -t to launch image loading on threads concurrently.

    Optionally, in addition to -t, specify -c <num> to override the number of threads.

- qimage_test_rhi

    Like qimage_test but initializes a QRhi for offscreen use and creates a
    QRhiTexture for each loaded QImage, and prints the elapsed time.

    Run with -m to also generate mipmaps after uploading. (although this may be
    misleading with APIs where mipmap generation is not performed by us but by
    the driver, and so may also happen at a later time; best to use this with
    Vulkan and D3D12)

    Backend overrides: pass -v for Vulkan, -D for D3D12.
