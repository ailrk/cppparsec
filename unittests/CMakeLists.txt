include_directories (${cppparsec_SOURCE_DIR}/include)
link_directories (${cppparsec_BINARY_DIR}/lib)

set(CMAKE_BUILD_TYPE Debug)
# Add executable that is built from the source files.
add_executable (unittest test.cc)
