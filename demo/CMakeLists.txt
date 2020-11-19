set(EXECUTABLE_OUTPUT_PATH "${PROJECT_BINARY_DIR}/bin")
add_executable(calculator-demo calculator.cc)
target_link_libraries(calculator-demo PRIVATE cppparsec)
  target_precompile_headers(calculator-demo PRIVATE ../cppparsec/cppparsec.h)

add_executable(json-demo json.cc)
target_link_libraries(json-demo PRIVATE cppparsec)
target_precompile_headers(json-demo PRIVATE ../cppparsec/cppparsec.h)

message("compielr is ${CMAKE_CXX_COMPILER}")


if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
  # set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
endif()

