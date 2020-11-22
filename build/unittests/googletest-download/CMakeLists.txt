cmake_minimum_required(VERSION 3.5)

project(googletest-download NONE)

include(ExternalProject)
externalproject_add(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG master
  SOURCE_DIR "/googletest-src"
  BINARY_DIR "/home/jimmy/newDisk/Repo/cppparsec/build/unittests/googletest-build"
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  TEST_COMMAND ""
)
