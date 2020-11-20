cmake_minimum_required(VERSION 3.10)
project(cppparsec)


# Check for C++11 support and set the compilation flag
# include(CheckCXXCompilerFlag)
# CHECK_CXX_COMPILER_FLAG("-std=c++17" COMPILER_SUPPORTS_CXX17)
# CHECK_CXX_COMPILER_FLAG("-std=c++1y" COMPILER_SUPPORTS_CXX1Y)
# if(COMPILER_SUPPORTS_CXX14)
# 	set(CMAKE_CXX_FAGS "${CMAKE_CXX_FLAGS} -std=c++17")
# elseif(COMPILER_SUPPORTS_CXX1Y)
# 	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
# else()
# 	message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++14 support. Please use a different C++ compiler.")
# endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

add_subdirectory(test)
