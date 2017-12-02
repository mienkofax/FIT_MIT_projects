cmake_minimum_required(VERSION 2.8.11)
project(sen C)

set(CMAKE_C_STANDARD 11)

find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB SOURCES
	AdafruitFingerPrint.c
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(senAPI ${SOURCES})

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	senAPI
	-Wl,--no-whole-archive
)
