cmake_minimum_required(VERSION 2.8.11)
project(bms2 C)

set(CMAKE_C_STANDARD 11)

find_library (POCO_FOUNDATION PocoFoundation)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_executable("bms2A" bms2A.cpp)

target_link_libraries("bms2A"
	-Wl,--whole-archive
	-Wl,--no-whole-archive
)

add_executable("bms2B" bms2B.cpp)

target_link_libraries("bms2B"
	-Wl,--whole-archive
	-Wl,--no-whole-archive
)
