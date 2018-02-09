cmake_minimum_required(VERSION 2.8.11)

find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB SOURCES
	AdafruitFingerPrint.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(sen-lib ${SOURCES})
