cmake_minimum_required(VERSION 3.5)

project(cppparsec VERSION 1.0
                  DESCRIPTION "a monadic parser combinator libary"
                  LANGUAGES CXX)

message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")

# options
option(CPPPARSEC_BUILD_FUZZER "Build fuzzer tests.")
option(CPPPARSEC_BUILD_TEST "Build unit tests")

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
if ("${CMAKE_CXX_FLAGS}" MATCHES ".*-fno-rtti.*")
  message("cppparsec: rtti is disabled")
endif()

# debug flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror")

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
endif()

# add test targets
if(CPPPARSEC_BUILD_FUZZER)
  message("cppparsec: Building fuzz tests")
  add_subdirectory(fuzzer)
endif()

if (CPPPARSEC_BUILD_TEST)
  message("cppparsec: Building unit tests")
  add_subdirectory(test)
endif()
