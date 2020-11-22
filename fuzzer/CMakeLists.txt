add_executable(fuzz-stream fuzz_stream.cc)
target_link_libraries(fuzz-stream cppparsec)

add_executable(fuzz-parser parser.cc)
target_link_libraries(fuzz-parser cppparsec)
