cmake_minimum_required(VERSION 3.5)

project (analyzer)

include (CheckCXXCompilerFlag)

set (CMAKE_CXX_FLAGS " ${CMAKE_CXX_FLAGS} -g -pedantic -Wall -Wextra")
add_definitions(-std=c++11)

set (SOURCE
	ArgumentParser.cpp
	ArgumentValidator.cpp
	PcapReaderFromFile.cpp
	PcapReaderFromVector.cpp
	LayerMessage.cpp
	LinkLayerMessageFactory.cpp
	NetworkLayerMessageFactory.cpp
	TransportLayerMessageFactory.cpp
	GenericLayerMessageFactory.cpp
	Statistics.cpp
	main.cpp
	)

include_directories(${CMAKE_SOURCE_DIR})

add_executable (${PROJECT_NAME} ${SOURCE})

target_link_libraries (
	${PROJECT_NAME}
	)
