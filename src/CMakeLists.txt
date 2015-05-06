cmake_minimum_required (VERSION 3.0)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(SOURCES
   cKwTable.c
   comments.c
   cOperTable.c
   javaOperTable.c
   javaKwTable.c
   misc_lexers.c
   quarry.c
   quarry_util.c
)

add_library(quarry SHARED   ${SOURCES})
add_library(squarry STATIC   ${SOURCES})

install (TARGETS quarry squarry DESTINATION lib)
install (FILES quarry.h DESTINATION include)
