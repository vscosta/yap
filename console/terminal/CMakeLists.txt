
cmake_minimum_required(VERSION 2.8.11)

option (QTCONSOLE "enable QT console" on)

if (QTCONSOLE)
    # Instruct CMake to run moc automatically when needed.
    set(CMAKE_AUTOMOC ON)

    # Find the  QtWidgets library
    macro_optional_find_package(Qt5Widgets on)

    macro_optional_find_package(Qt5Core on)

endif(QTCONSOLE)


if (Qt5Widgets_FOUND)
  set ( QTCONSOLE_SOURCES
console.cpp
console.h
main.cpp
mainwindow.h
mainwindow.ui
mainwindow.cpp
settingsdialog.h
settingsdialog.ui
settingsdialog.cpp
terminal.pro
terminal.qrc
images/application-exit.png
images/clear.png
images/connect.png
images/disconnect.png
images/settings.png

	)

    # Find includes in corresponding build directories
    set(CMAKE_INCLUDE_CURRENT_DIR ON)

    qt5_wrap_ui(ui_settingsdialog.h settingsdialog.ui)

    qt5_wrap_ui(ui_mainwindow.h mainwindow.ui)

    qt5_add_resources(qrc_terminal.cpp terminal.qrc)

    add_executable (qtyap ${QTCONSOLE_SOURCES} qrc_terminal.cpp)

include_directories(../../CXX)

  set_target_properties (qtyap PROPERTIES OUTPUT_NAME qtyap CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)

  target_link_libraries(qtyap Yap++)


    qt5_use_modules(qtyap Widgets)


  install(TARGETS  qtyap
     RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BIINDIR}
    )

ENDIF(Qt5Widgets_FOUND)
