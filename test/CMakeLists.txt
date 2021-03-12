# include header
include_directories(${cppparsec_SOURCE_DIR}/include)
link_directories(${cppparsec_BINARY_DIR}/lib)

# include catch2
add_subdirectory(external/catch2 ${cppparsec_BINARY_DIR}/lib)
include_directories(external/catch2)

# precompiled header
add_library(main STATIC main.cc)
add_executable(test_core test_core.cc)
add_executable(test_combinator test_combinator.cc)
add_executable(test_errorhandling test_errorhandling.cc)

target_link_libraries(test_core main)
target_link_libraries(test_combinator main)
target_link_libraries(test_errorhandling main)
# target_link_libraries(tests Catch2::Catch2)

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

target_precompile_headers(test_core PRIVATE external/catch2/single_include/catch2/catch.hpp)
target_precompile_headers(test_combinator PRIVATE external/catch2/single_include/catch2/catch.hpp)
target_precompile_headers(test_errorhandling PRIVATE external/catch2/single_include/catch2/catch.hpp)

# add_custom_target(test "tests" "-d yes")
