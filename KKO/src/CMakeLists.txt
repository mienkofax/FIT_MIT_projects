cmake_minimum_required(VERSION 2.8.11)
project(kko CXX)

file(GLOB SOURCES
	${PROJECT_SOURCE_DIR}/gif2bmp.c
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(kko-lib ${SOURCES})

add_executable(${PROJECT_NAME} main.c)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	kko-lib
	-Wl,--no-whole-archive
)
