cmake_minimum_required(VERSION 3.28)
project(neutronium)

set(CMAKE_CXX_STANDARD 23)

include_directories(include)

file(GLOB_RECURSE LIB_SOURCES "src/*.cpp")
list(REMOVE_ITEM LIB_SOURCES "${CMAKE_SOURCE_DIR}/src/main.cpp")

add_library(neutronium_lib ${LIB_SOURCES})

add_executable(neutronium src/main.cpp)

target_link_libraries(neutronium PRIVATE neutronium_lib)

# === GoogleTest setup ===
include(FetchContent)
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/release-1.12.1.zip
)
FetchContent_MakeAvailable(googletest)

enable_testing()

file(GLOB_RECURSE TEST_SOURCES
        "tests/*.cpp"
)

add_executable(tests ${TEST_SOURCES})

target_link_libraries(tests PRIVATE
        neutronium_lib
        gtest_main
)

include(GoogleTest)
gtest_discover_tests(tests)