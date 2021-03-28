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

# header only lib
include(CMakePackageConfigHelpers)

add_library(cppparsec INTERFACE)
add_library(cppparsec::cppparsec ALIAS cppparsec)
target_include_directories(cppparsec INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/cppparsec>)

if (CPPPARSEC_BUILD_FUZZER AND CPPPARSEC_BUILD_TEST)
  message(FATAL_ERROR
    "Can't build fuzzer and test together. Please choose one.")
endif()

# test targets
if(CPPPARSEC_BUILD_FUZZER)
  message("cppparsec: Building fuzz tests")
  add_subdirectory(fuzzer)
endif()

if (CPPPARSEC_BUILD_TEST)
  message("cppparsec: Building unit tests")
  add_subdirectory(test)
endif()
