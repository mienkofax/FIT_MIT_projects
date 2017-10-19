cmake_minimum_required(VERSION 2.8.11)

add_definitions(-std=c++11 -Wall -pedantic -Wextra)

add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(../common subproject/common)
