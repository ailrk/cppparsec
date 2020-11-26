include_directories(${cppparsec_SOURCE_DIR}/include)
link_directories(${cppparsec_BINARY_DIR}/lib)

# include catch2
add_subdirectory(${cppparsec_SOURCE_DIR}/extern/catch2 ${cppparsec_BINARY_DIR}/lib)
include_directories(${cppparsec_SOURCE_DIR}/extern/catch2)

target_link_libraries(tests Catch2::Catch2)
add_executable(test test.cc)
