cmake_minimum_required(VERSION 2.8.11)
project(bms2 CXX)

file(GLOB SOURCES
	${PROJECT_SOURCE_DIR}/AbstractModulator.cpp
	${PROJECT_SOURCE_DIR}/Demodulator.cpp
	${PROJECT_SOURCE_DIR}/Modulator.cpp
	${PROJECT_SOURCE_DIR}/ModulatorException.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(bms2-lib ${SOURCES})

add_executable("bms2A" ${PROJECT_SOURCE_DIR}/bms2A.cpp)
target_link_libraries("bms2A"
	-Wl,--whole-archive
	bms2-lib
	${PROJECT_SOURCE_DIR}/libsndfile.a
	-Wl,--no-whole-archive
)

add_executable("bms2B" bms2B.cpp)
target_link_libraries("bms2B"
	-Wl,--whole-archive
	bms2-lib
	${PROJECT_SOURCE_DIR}/libsndfile.a
	-Wl,--no-whole-archive
)
