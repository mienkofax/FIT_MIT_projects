cmake_minimum_required(VERSION 2.8.11)
project(CommonPeto CXX)

file(GLOB COMMON_SOURCES
	${PROJECT_SOURCE_DIR}/TapOutputter.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(${PROJECT_NAME} SHARED ${COMMON_SOURCES})
