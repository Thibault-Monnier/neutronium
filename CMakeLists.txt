cmake_minimum_required(VERSION 3.28)

if (DEFINED COMPILER)
    if (COMPILER STREQUAL "gcc")
        set(CMAKE_C_COMPILER gcc)
        set(CMAKE_CXX_COMPILER g++)
    elseif (COMPILER STREQUAL "clang")
        set(CMAKE_C_COMPILER clang)
        set(CMAKE_CXX_COMPILER clang++)
    else ()
        message(FATAL_ERROR "Unsupported compiler: ${COMPILER}. Supported compilers are 'gcc' and 'clang'.")
    endif ()
endif ()

project(neutronium)

include(FetchContent)

# --- Compiler mismatch warning (after project) ---
if (DEFINED COMPILER)
    if (COMPILER STREQUAL "gcc" AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        message(WARNING
                "You previously compiled with ${CMAKE_CXX_COMPILER_ID}, but set COMPILER=gcc. "
                "Not changing compiler. Delete CMakeCache.txt to switch.")
    elseif (COMPILER STREQUAL "clang" AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message(WARNING
                "You previously compiled with ${CMAKE_CXX_COMPILER_ID}, but set COMPILER=clang. "
                "Not changing compiler. Delete CMakeCache.txt to switch.")
    endif ()
endif ()

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif ()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -Wall -Wextra -Wpedantic -Wshadow -fsanitize=address,undefined -fno-sanitize-recover=all -g3 -fno-omit-frame-pointer")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-fdiagnostics-color=always -fdiagnostics-show-template-tree -fstandalone-debug)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compile_options(-fdiagnostics-color=always)
endif ()

if (COVERAGE)
    message(STATUS "Building with code coverage flags")
    add_compile_options(--coverage)
    add_link_options(--coverage)
endif ()

include_directories(include)

file(GLOB_RECURSE LIB_SOURCES "src/*.cpp")
list(REMOVE_ITEM LIB_SOURCES "${CMAKE_SOURCE_DIR}/src/main.cpp")
file(GLOB_RECURSE HEADER_FILES "include/*.hpp" "include/*.h")

add_compile_definitions(PROJECT_ROOT_DIR="${CMAKE_SOURCE_DIR}")

add_library(neutronium_lib ${LIB_SOURCES} ${HEADER_FILES})

add_executable(neutronium src/main.cpp)

# === cxxopts setup ===
FetchContent_Declare(
        cxxopts
        GIT_REPOSITORY https://github.com/jarro2783/cxxopts.git
        GIT_TAG v3.3.1
)
FetchContent_MakeAvailable(cxxopts)

target_link_libraries(neutronium_lib PRIVATE cxxopts)

# === magic_enum setup ===
FetchContent_Declare(
        magic_enum
        GIT_REPOSITORY https://github.com/Neargye/magic_enum.git
        GIT_TAG v0.9.7
)
FetchContent_MakeAvailable(magic_enum)

target_link_libraries(neutronium_lib PRIVATE magic_enum)

target_link_libraries(neutronium PRIVATE neutronium_lib)

# === GoogleTest setup ===
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.17.0
)
FetchContent_MakeAvailable(googletest)

enable_testing()

file(GLOB_RECURSE TEST_SOURCES "tests/*.cpp")

add_executable(tests ${TEST_SOURCES})

target_link_libraries(tests PRIVATE
        neutronium_lib
        gtest_main
)

include(GoogleTest)
gtest_discover_tests(tests)