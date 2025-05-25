cmake_minimum_required(VERSION 3.28)
project(neutronium)

include(FetchContent)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif ()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -Wall -Wextra -Wpedantic -Wshadow -fsanitize=address,undefined -g -fno-omit-frame-pointer")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

if (COVERAGE)
    message(STATUS "Building with code coverage flags")
    add_compile_options(--coverage)
    add_link_options(--coverage)
endif ()

include_directories(include)

file(GLOB_RECURSE LIB_SOURCES "src/*.cpp")
list(REMOVE_ITEM LIB_SOURCES "${CMAKE_SOURCE_DIR}/src/main.cpp")

add_library(neutronium_lib ${LIB_SOURCES})

add_executable(neutronium src/main.cpp)

# === cxxopts setup ===
FetchContent_Declare(
        cxxopts
        GIT_REPOSITORY https://github.com/jarro2783/cxxopts.git
        GIT_TAG v3.3.0
)
FetchContent_MakeAvailable(cxxopts)

target_link_libraries(neutronium_lib PRIVATE cxxopts)
target_link_libraries(neutronium PRIVATE neutronium_lib)

# === GoogleTest setup ===
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/release-1.12.1.zip
)
FetchContent_MakeAvailable(googletest)

enable_testing()

file(GLOB_RECURSE TEST_SOURCES "tests/*.cpp")

add_executable(tests ${TEST_SOURCES})

target_link_libraries(tests PRIVATE
        neutronium_lib
        gtest_main
)
target_compile_definitions(tests PRIVATE
        PROJECT_ROOT_DIR="${CMAKE_SOURCE_DIR}"
)

include(GoogleTest)
gtest_discover_tests(tests)