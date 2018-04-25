cmake_minimum_required(VERSION 2.8.11)
project(PDS CXX)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

file(GLOB SOURCES
	${PROJECT_SOURCE_DIR}/PcapUtil.cpp
	${PROJECT_SOURCE_DIR}/DHCPMsg.cpp
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_library(PDS-lib ${SOURCES})

add_executable(${PROJECT_NAME} main.cpp)

set(LIBS
	${PCAP_LIBRARY}
)


target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	PDS-lib
	-Wl,--no-whole-archive
	${LIBS}
)
