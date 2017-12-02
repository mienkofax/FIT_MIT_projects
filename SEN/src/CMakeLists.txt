cmake_minimum_required(VERSION 2.8.11)
project(sen C)

set(CMAKE_C_STANDARD 11)

find_library (POCO_FOUNDATION PocoFoundation)

include_directories(
	${PROJECT_SOURCE_DIR}
)

add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME}
	-Wl,--whole-archive
	-Wl,--no-whole-archive
)
