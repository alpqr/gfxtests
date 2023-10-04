cmake_minimum_required(VERSION 3.20)
project(gltfgeom_rhiwidget LANGUAGES CXX)

find_package(Qt6 COMPONENTS Core Gui Widgets ShaderTools)

qt_add_executable(gltfgeom_rhiwidget
    main.cpp
)

target_link_libraries(gltfgeom_rhiwidget PRIVATE
    Qt::Core
    Qt::Gui
    Qt::GuiPrivate
    Qt::Widgets
)

qt_add_shaders(gltfgeom_rhiwidget "shaders"
    PREFIX
        /shaders
    FILES
        basic.vert
        basic.frag
)
