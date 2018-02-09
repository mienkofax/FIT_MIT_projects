cmake_minimum_required(VERSION 2.8.11)
project(senTestSuite CXX)

add_definitions(-std=c++11 -Wall -pedantic -Wextra)

find_library(CPP_UNIT cppunit)
find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB TEST_SOURCES
	AdafruitFingerPrintTest.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
	${PROJECT_SOURCE_DIR}/../src
	${PROJECT_SOURCE_DIR}/../../common
)

add_library(senTestSuite-lib ${TEST_SOURCES})

add_executable(${PROJECT_NAME} ../../common/test.cpp)

set(LIBS
	${CPP_UNIT}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	sen-lib
	common-lib
	senTestSuite-lib
	-Wl,--no-whole-archive
	${LIBS}
)
