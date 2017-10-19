cmake_minimum_required(VERSION 2.8.11)
project(p1 CXX)

find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB SOURCES
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(ExampleCmake ${SOURCES})

add_executable(${PROJECT_NAME} main.cpp)

set(LIBS
	${POCO_FOUNDATION}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	ExampleCmake
	-Wl,--no-whole-archive
	${LIBS}
)
