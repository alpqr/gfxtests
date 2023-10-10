cmake_minimum_required(VERSION 3.20)
project(scene LANGUAGES CXX)

find_package(Qt6 COMPONENTS Core Gui Widgets ShaderTools)

qt_add_executable(scene
    main.cpp
)

target_link_libraries(scene PRIVATE
    Qt::Core
    Qt::Gui
    Qt::GuiPrivate
    Qt::Widgets
)

qt_add_shaders(scene "shaders"
    PREFIX
        /
    FILES
        basic.vert
        basic.frag
)

target_sources(scene PRIVATE
    ../3rdparty/imgui/imgui.cpp
    ../3rdparty/imgui/imgui_draw.cpp
    ../3rdparty/imgui/imgui_tables.cpp
    ../3rdparty/imgui/imgui_widgets.cpp
    ../3rdparty/imgui/imgui_demo.cpp
    qrhiimgui.cpp
    qrhiimgui.h
)

target_include_directories(scene PRIVATE
    ../3rdparty/imgui
)

qt6_add_shaders(scene "imgui_shaders"
    PREFIX
        /
    FILES
        imgui.vert
        imgui.frag
)

qt6_add_resources(scene "imgui_resources"
    PREFIX
        /
    BASE
        ../3rdparty/fonts
    FILES
        ../3rdparty/fonts/RobotoMono-Medium.ttf
)
