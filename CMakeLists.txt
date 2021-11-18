cmake_minimum_required(VERSION 3.21)

project(introspective)
include(GNUInstallDirs)


# Leave either of them empty to disable compilation of Lua example.
set(INTSPEC_LUA_HEADER_PATH CACHE PATH "Path to Lua headers for example executable")
set(INTSPEC_LUA_LIBRARY_PATH CACHE PATH "Path to the compiled Lua Library files for example executable")


# Leave either of them empty to disable compilation of Wren example.
set(INTSPEC_WREN_HEADER_PATH CACHE PATH "Path to Wren headers for example executable")
set(INTSPEC_WREN_LIBRARY_PATH CACHE PATH "Path to the compiled Wren Library files for example executable")


# Leave either of them empty to disable compilation of Julia example.
set(INTSPEC_JULIA_HEADER_PATH CACHE PATH "Path to Julia headers for example executable")
set(INTSPEC_JULIA_LIBRARY_PATH CACHE PATH "Path to the compiled Julia Library files for example executable")


set(INTSPEC_HEADERS "include/introspective.h")

add_library(introspective INTERFACE)

set_target_properties(introspective PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED TRUE
    PUBLIC_HEADER "${INTSPEC_HEADERS}")

target_include_directories(introspective
    INTERFACE ${PROJECT_SOURCE_DIR}/include)

if((IS_DIRECTORY ${INTSPEC_LUA_HEADER_PATH}) AND (IS_DIRECTORY ${INTSPEC_LUA_LIBRARY_PATH}))
    add_executable(luaexample example/LuaExample.cpp)
    set_target_properties(luaexample PROPERTIES
        CXX_STANDARD 20
        CXX_STANDARD_REQUIRED 20
        RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/example)
    target_include_directories(luaexample PUBLIC ${INTSPEC_LUA_HEADER_PATH})
    find_library(LUA_LIBRARY lua PATHS ${INTSPEC_LUA_LIBRARY_PATH})
    target_link_libraries(luaexample introspective ${LUA_LIBRARY})
endif()

if((IS_DIRECTORY ${INTSPEC_WREN_HEADER_PATH}) AND (IS_DIRECTORY ${INTSPEC_WREN_LIBRARY_PATH}))
    add_executable(wrenexample example/WrenExample.cpp)
    set_target_properties(wrenexample PROPERTIES
        CXX_STANDARD 20
        CXX_STANDARD_REQUIRED 20
        RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/example)
    target_include_directories(wrenexample PUBLIC ${INTSPEC_WREN_HEADER_PATH})
    find_library(WREN_LIBRARY wren PATHS ${INTSPEC_WREN_LIBRARY_PATH})
    target_link_libraries(wrenexample introspective ${WREN_LIBRARY})
endif()

if((IS_DIRECTORY ${INTSPEC_JULIA_HEADER_PATH}) AND (IS_DIRECTORY ${INTSPEC_JULIA_LIBRARY_PATH}))
    add_executable(juliaexample example/JuliaExample.cpp)
    set_target_properties(juliaexample PROPERTIES
        CXX_STANDARD 20
        CXX_STANDARD_REQUIRED 20
        RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/example)
    target_include_directories(juliaexample PUBLIC ${INTSPEC_JULIA_HEADER_PATH})
    find_library(JULIA_LIBRARY julia PATHS ${INTSPEC_JULIA_LIBRARY_PATH})
    target_link_libraries(juliaexample introspective ${JULIA_LIBRARY})
endif()

