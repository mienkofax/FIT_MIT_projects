cmake_minimum_required(VERSION 2.8.11)
project(test-suite CXX)

find_library(CPP_UNIT cppunit)
find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB TEST_SOURCES
)

include_directories(
	${PROJECT_SOURCE_DIR}
	${PROJECT_SOURCE_DIR}/../src
	${PROJECT_SOURCE_DIR}/../../common
)

add_library(TestSuite ${TEST_SOURCES})

add_executable(${PROJECT_NAME} ../../common/test.cpp)

set(LIBS
	${CPP_UNIT}
	${POCO_FOUNDATION}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	CommonPeto
	TestSuite
	ExampleCmake
	-Wl,--no-whole-archive
	${LIBS}
)
