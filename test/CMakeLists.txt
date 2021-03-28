set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(EXECUTABLE_OUTPUT_PATH "${PROJECT_BINARY_DIR}/bin")

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
endif()


# include header
include_directories(${cppparsec_SOURCE_DIR}/cppparsec)
link_directories(${cppparsec_BINARY_DIR}/lib)

# include catch2
find_package(Catch2 REQUIRED)

add_library(main STATIC main.cc)
add_executable(test_core test_core.cc)
add_executable(test_combinator test_combinator.cc)
add_executable(test_errorhandling test_errorhandling.cc)

target_link_libraries(main Catch2::Catch2)
target_link_libraries(test_core main)
target_link_libraries(test_combinator main)
target_link_libraries(test_errorhandling main)

# debug flags

set(TEST_COMPILER_FLAG "")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wall")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Wextra")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -pedantic")
set(TEST_COMPILER_FLAG "${TEST_COMPILER_FLAG} -Werror")
set_target_properties(
  test_core test_combinator test_errorhandling
  PROPERTIES
  COMPILE_FLAGS "${TEST_COMPILER_FLAG}")


# precompiled header
target_compile_definitions(
  test_core PRIVATE CATCH_CONFIG_FAST_COMPILE
  CATCH_CONFIG_DISABLE_MATCHERS
)
target_compile_definitions(
  test_combinator PRIVATE CATCH_CONFIG_FAST_COMPILE
  CATCH_CONFIG_DISABLE_MATCHERS
)
target_compile_definitions(
  test_errorhandling PRIVATE CATCH_CONFIG_FAST_COMPILE
  CATCH_CONFIG_DISABLE_MATCHERS
)

# target_precompile_headers(test_core PRIVATE external/catch2/single_include/catch2/catch.hpp)
# target_precompile_headers(test_combinator PRIVATE external/catch2/single_include/catch2/catch.hpp)
# target_precompile_headers(test_errorhandling PRIVATE external/catch2/single_include/catch2/catch.hpp)

# add_custom_target(test "tests" "-d yes")
