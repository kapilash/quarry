cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(SOURCES
  QReader.cpp
  QReader.h
  QInternal.h
  QContext.cpp
  QuarryMaps.cpp
)
find_package(Boost
  1.57.0
   COMPONENTS
  unit_test_framework
  test_exec_monitor
   REQUIRED
 )

set(INCFOLDER ${PROJECT_SOURCE_DIR}/inc )

include_directories(${INCFOLDER} ${Boost_INCLUDE_DIRS})

add_library(quarry SHARED   ${SOURCES})
