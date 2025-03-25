cmake_minimum_required(VERSION 3.28)
project(neutronium)

set(CMAKE_CXX_STANDARD 23)

include_directories(include)

file (GLOB_RECURSE SOURCES "src/*.cpp")

add_executable(neutronium ${SOURCES})
