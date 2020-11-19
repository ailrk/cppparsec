include_directories($(cppparsec_SOURCE_DIR)/include)
link_directories($(cppparsec_BINARY_DIR)/lib)

add_executable (test test.cc)
