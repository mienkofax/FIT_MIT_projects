cmake_minimum_required(VERSION 2.8.11)
project(bms2-test-suite CXX)

add_definitions(-std=c++11 -Wall -pedantic -Wextra)

find_library(CPP_UNIT cppunit)
find_library (POCO_FOUNDATION PocoFoundation)

include_directories(
	${PROJECT_SOURCE_DIR}
	${PROJECT_SOURCE_DIR}/../src
	${PROJECT_SOURCE_DIR}/../../common
)

add_executable(${PROJECT_NAME} ../../common/test.cpp)

set(LIBS
	${CPP_UNIT}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	CommonPeto
	-Wl,--no-whole-archive
	${LIBS}
)
