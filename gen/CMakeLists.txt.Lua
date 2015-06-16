cmake_minimum_required (VERSION 3.0)

project(lua VERSION 5.3 LANGUAGES C CXX)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


set (SOURCES
    lapi.c
    lapi.h
    lauxlib.c
    lauxlib.h
    lbaselib.c
    lbitlib.c
    lcode.c
    lcode.h
    lcorolib.c
    lctype.c
    lctype.h
    ldblib.c
    ldebug.c
    ldebug.h
    ldo.c
    ldo.h
    ldump.c
    lfunc.c
    lfunc.h
    lgc.c
    lgc.h
    linit.c
    liolib.c
    llex.c
    llex.h
    llimits.h
    lmathlib.c
    lmem.c
    lmem.h
    loadlib.c
    lobject.c
    lobject.h
    lopcodes.c
    lopcodes.h
    loslib.c
    lparser.c
    lparser.h
    lprefix.h
    lstate.c
    lstate.h
    lstring.c
    lstring.h
    lstrlib.c
    ltable.c
    ltable.h
    ltablib.c
    ltm.c
    ltm.h
    lua.h
    lua.hpp
    luaconf.h
    lualib.h
    lundump.c
    lundump.h
    lutf8lib.c
    lvm.c
    lvm.h
    lzio.c
    lzio.h
 )

add_library(lua53 STATIC   ${SOURCES})

add_executable(lua "lua.c")

add_executable(luac "luac.c")

target_link_libraries(lua lua53)

target_link_libraries(luac lua53)

install (TARGETS lua53  DESTINATION lib)
install (FILES lua.h lua.hpp lualib.h luaauxlib.h DESTINATION include)
