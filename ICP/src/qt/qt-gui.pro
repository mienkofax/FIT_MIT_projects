#-------------------------------------------------
#
# Project created by QtCreator 2016-05-04T23:45:19
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

QMAKE_CXXFLAGS += -std=c++11

TARGET = hra2016
TEMPLATE = app
LIBS += ../core/othello-lib.a
INCLUDEPATH += ../core

SOURCES += main.cpp\
    gui.cpp \
    square.cpp \
    game.cpp \
    ../core/GameBoard.cpp \
    ../core/GameData.cpp \
    ../core/GameManager.cpp \
    ../core/Player.cpp \
    ../core/Strategy.cpp

HEADERS  += gui.h \
    ui_gui.h \
    square.h \
    game.h \
    ../core/GameBoard.h \
    ../core/GameData.h \
    ../core/GameManager.h \
    ../core/Player.h \
    ../core/Strategy.h

FORMS    += gui.ui
