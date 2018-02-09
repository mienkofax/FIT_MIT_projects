cmake_minimum_required(VERSION 2.8.11)
project(bms1 CXX)

find_library (POCO_FOUNDATION PocoFoundation)

file(GLOB SOURCES
	${PROJECT_SOURCE_DIR}/Application.cpp
	${PROJECT_SOURCE_DIR}/BTSInfo.cpp
	${PROJECT_SOURCE_DIR}/CSVParser.cpp
	${PROJECT_SOURCE_DIR}/GPSCoordinate.cpp
	${PROJECT_SOURCE_DIR}/HataModel.cpp
	${PROJECT_SOURCE_DIR}/Location.cpp
	${PROJECT_SOURCE_DIR}/UTMWrapper.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(bms1-lib ${SOURCES})

add_executable(${PROJECT_NAME} main.cpp)

set(LIBS
	${POCO_FOUNDATION}
)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	bms1-lib
	-Wl,--no-whole-archive
	${LIBS}
)
