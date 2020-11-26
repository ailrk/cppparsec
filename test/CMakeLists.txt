# include header
include_directories(${cppparsec_SOURCE_DIR}/include)
link_directories(${cppparsec_BINARY_DIR}/lib)

# include catch2
add_subdirectory(external/catch2 ${cppparsec_BINARY_DIR}/lib)
include_directories(external/catch2)

add_executable(test test.cc)
target_link_libraries(test Catch2::Catch2)
