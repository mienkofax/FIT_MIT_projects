cmake_minimum_required(VERSION 2.8.11)
project(kkoTestSuite CXX)

find_library(CPP_UNIT cppunit)

file(GLOB TEST_SOURCES
	${PROJECT_SOURCE_DIR}/gif2bmpTest.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
	${PROJECT_SOURCE_DIR}/../src
	${PROJECT_SOURCE_DIR}/../../common
)

add_library(kkoTestSuite-lib ${TEST_SOURCES})

add_executable(${PROJECT_NAME} ../../common/test.cpp)

set(LIBS
	${CPP_UNIT}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	common-lib
	kkoTestSuite-lib
	kko-lib
	-Wl,--no-whole-archive
	${LIBS}
)
