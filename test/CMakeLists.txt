# include header
include_directories(${cppparsec_SOURCE_DIR}/include)
link_directories(${cppparsec_BINARY_DIR}/lib)

# include catch2
add_subdirectory(external/catch2 ${cppparsec_BINARY_DIR}/lib)
include_directories(external/catch2)

# precompiled header
add_library(main STATIC main.cc)
add_executable(tests tests.cc)

target_link_libraries(tests main)
# target_link_libraries(tests Catch2::Catch2)

target_compile_definitions(
  tests PRIVATE CATCH_CONFIG_FAST_COMPILE
  CATCH_CONFIG_DISABLE_MATCHERS
)

target_precompile_headers(tests PRIVATE external/catch2/single_include/catch2/catch.hpp)

add_custom_target(test "testss" "-d yes")
