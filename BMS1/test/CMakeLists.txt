cmake_minimum_required(VERSION 2.8.11)
project(bms1TestSuite CXX)

find_library(CPP_UNIT cppunit)
find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB TEST_SOURCES
	${PROJECT_SOURCE_DIR}/ApplicationTest.cpp
	${PROJECT_SOURCE_DIR}/CSVParserTest.cpp
	${PROJECT_SOURCE_DIR}/GPSCoordinateTest.cpp
	${PROJECT_SOURCE_DIR}/HataModelTest.cpp
	${PROJECT_SOURCE_DIR}/LocationTest.cpp
	${PROJECT_SOURCE_DIR}/UTMWrapperTest.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
	${PROJECT_SOURCE_DIR}/../src
	${PROJECT_SOURCE_DIR}/../../common
)

add_library(bms1TestSuite-lib ${TEST_SOURCES})

add_executable(${PROJECT_NAME} ../../common/test.cpp)

set(LIBS
	${CPP_UNIT}
	${POCO_FOUNDATION}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	common-lib
	bms1TestSuite-lib
	bms1-lib
	-Wl,--no-whole-archive
	${LIBS}
)
