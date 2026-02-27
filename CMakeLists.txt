cmake_minimum_required(VERSION 3.28)

# --- Compiler selection ---
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

project(neutronium LANGUAGES CXX DESCRIPTION "Neutronium language compiler")

# --- Compiler mismatch warning ---
if (DEFINED COMPILER)
    if (COMPILER STREQUAL "gcc" AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        message(WARNING "You previously compiled with ${CMAKE_CXX_COMPILER_ID}, but set COMPILER=gcc. Delete CMakeCache.txt to switch.")
    elseif (COMPILER STREQUAL "clang" AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message(WARNING "You previously compiled with ${CMAKE_CXX_COMPILER_ID}, but set COMPILER=clang. Delete CMakeCache.txt to switch.")
    endif ()
endif ()

# --- Build type ---
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
    message(STATUS "No build type specified. Defaulting to Debug.")
endif ()

# --- Language standard ---
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# --- Compiler flags ---
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -Wall -Wextra -Wshadow -fsanitize=address,undefined -fno-sanitize-recover=all -g3 -fno-omit-frame-pointer -fno-lto")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O3 -Wall -Wextra -Wshadow -g3 -fno-omit-frame-pointer -fno-lto -march=native -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -Wall -Wextra -Wshadow -fno-lto -march=native -DNDEBUG")
add_link_options(-fno-lto)

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-stdlib=libc++ -fdiagnostics-color=always -fdiagnostics-show-template-tree -fstandalone-debug)
    add_link_options(-stdlib=libc++ -lc++ -lc++abi)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compile_options(-fdiagnostics-color=always)
    message(WARNING "Using GNU compiler. Consider using Clang which is better tested and optimized.")
endif ()

if (COVERAGE)
    message(STATUS "Building with code coverage flags")
    add_compile_options(--coverage)
    add_link_options(--coverage)
endif ()

# --- Sources ---
file(GLOB_RECURSE LIB_SOURCES "src/*.cpp")
list(REMOVE_ITEM LIB_SOURCES "${CMAKE_SOURCE_DIR}/src/driver/main.cpp")

add_compile_definitions(PROJECT_ROOT_DIR="${CMAKE_SOURCE_DIR}")

add_library(neutronium_lib ${LIB_SOURCES})
target_include_directories(neutronium_lib PUBLIC ${CMAKE_SOURCE_DIR}/src)

# --- Executable ---
add_executable(neutronium src/driver/main.cpp)
target_link_libraries(neutronium PRIVATE neutronium_lib)

# --- External libraries ---
include(FetchContent)

FetchContent_Declare(
        cxxopts
        GIT_REPOSITORY https://github.com/jarro2783/cxxopts.git
        GIT_TAG v3.3.1
        GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(cxxopts)

FetchContent_Declare(
        magic_enum
        GIT_REPOSITORY https://github.com/Neargye/magic_enum.git
        GIT_TAG v0.9.7
        GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(magic_enum)

target_link_libraries(neutronium_lib PUBLIC cxxopts magic_enum)

# --- Tests ---
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.17.0
        GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(googletest)

enable_testing()

file(GLOB_RECURSE TEST_SOURCES "tests/*.cpp")
add_executable(tests ${TEST_SOURCES})
target_link_libraries(tests PRIVATE neutronium_lib gtest_main)

include(GoogleTest)
gtest_discover_tests(tests)
