add_executable(fuzz-stream fuzz_stream.cc)
target_link_libraries(fuzz-stream PRIVATE cppparsec)

# add_executable(fuzz-parser fuzz_parser.cc)
# target_link_libraries(fuzz-parser cppparsec)

message("compiler is ${CMAKE_CXX_COMPILER}")

if (NOT "${CMAKE_CXX_COMPILER}" MATCHES ".*clang\\+\\+")
  message(FATAL_ERROR "Compiling with fuzzer support requires clang++")
endif()

set(FUZZER_COMPILER_FLAGS "")
set(FUZZER_COMPILER_FLAGS "${FUZZER_COMPILER_FLAGS} -fsanitize=fuzzer")

set(FUZZER_LINKER_FLAGS"")
set(FUZZER_LINKER_FLAGS "${FUZZER_LINKER_FLAGS} -fsanitize=fuzzer")

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  set_target_properties(fuzz-stream
      PROPERTIES
      COMPILE_FLAGS "${FUZZER_COMPILER_FLAGS}"
      LINK_FLAGS "${FUZZER_LINKER_FLAGS}"
    )
endif()
