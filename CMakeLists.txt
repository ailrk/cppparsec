cmake_minimum_required(VERSION 3.10)
project(cppparsec)

include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std-c++17" COMPILER_SUPPORTS_CXX17)
check_cxx_compiler_flag("-std-c++1x" COMPILER_SUPPORTS_CXX1Y)

if(COMPILER_SUPPORTS_CXX17)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
elseif(COMPILER_SUPPORTS_CXX1Y)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
else()
  message(STATUS "The compiler ${CMAKE_CXX_COMPILER} a has no c++17 support.")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

add_subdirectory(test)
