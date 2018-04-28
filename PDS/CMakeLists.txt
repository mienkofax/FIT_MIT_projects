cmake_minimum_required(VERSION 2.8.11)
project(PDS CXX)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(LIBS
	${PCAP_LIBRARY}
)

include_directories(
	${PROJECT_SOURCE_DIR}
)

file(GLOB SOURCES_STARVE
	${PROJECT_SOURCE_DIR}/PcapUtil.cpp
	${PROJECT_SOURCE_DIR}/ClientMessage.cpp
	${PROJECT_SOURCE_DIR}/Common.cpp
)

add_library(PDS-starve-lib ${SOURCES_STARVE})
add_executable("pds-dhcpstarve" ${PROJECT_SOURCE_DIR}/pds-dhcpstarve.cpp)
target_link_libraries("pds-dhcpstarve"
	-Wl,--whole-archive
	PDS-starve-lib
	-Wl,--no-whole-archive
	${LIBS}
)

file(GLOB SOURCES_ROGUE
	${PROJECT_SOURCE_DIR}/PcapUtil.cpp
	${PROJECT_SOURCE_DIR}/DHCPMsg.cpp
)

add_library(PDS-rogue-lib ${SOURCES_ROGUE})
add_executable("pds-dhcprogue" ${PROJECT_SOURCE_DIR}/pds-dhcprogue.cpp)
target_link_libraries("pds-dhcprogue"
-Wl,--whole-archive
PDS-rogue-lib
-Wl,--no-whole-archive
${LIBS}
)


