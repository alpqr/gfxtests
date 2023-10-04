cmake_minimum_required(VERSION 3.20)
project(qimage_test LANGUAGES CXX)

find_package(Qt6 COMPONENTS Core Gui)

qt_add_executable(qimage_test
    main.cpp
)

target_link_libraries(qimage_test PRIVATE
    Qt::Core
    Qt::Gui
)
