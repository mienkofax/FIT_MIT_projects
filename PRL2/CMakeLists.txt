cmake_minimum_required (VERSION 2.8.11)
project(PRL2)

set(CMAKE_CXX_STANDARD 11)

find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})

add_executable(pro pro.cpp)
target_link_libraries(pro ${MPI_LIBRARIES})

if(MPI_COMPILE_FLAGS)
set_target_properties(pro PROPERTIES
COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if(MPI_LINK_FLAGS)
set_target_properties(pro PROPERTIES
LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()

