cmake_minimum_required(VERSION 3.21)

project(introspective)

include(GNUInstallDirs)

option(INTSPEC_BUILD_EXAMPLES "Build examples in the example/ folder" ON)

set(INTSPEC_EXAMPLES_LUA_HEADER_PATH CACHE PATH "Path to Lua headers for example executable")
set(INTSPEC_EXAMPLES_WREN_HEADER_PATH CACHE PATH "Path to Wren headers for example executable")

set(INTSPEC_HEADERS "include/introspective.h")

add_library(introspective INTERFACE)

set_target_properties(introspective PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED TRUE
    PUBLIC_HEADER "${INTSPEC_HEADERS}")

target_include_directories(introspective
    INTERFACE ${PROJECT_SOURCE_DIR}/include)

if(INTSPEC_BUILD_EXAMPLES)
    add_executable(luaexample example/LuaExample.cpp)
    add_executable(wrenexample example/WrenExample.cpp)

    set_target_properties(luaexample wrenexample PROPERTIES
        CXX_STANDARD 20
        CXX_STANDARD_REQUIRED 20
        RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/example)

    find_library(LUA_LIBRARY lua)
    find_library(WREN_LIBRARY wren)

    target_include_directories(luaexample PUBLIC ${INTSPEC_EXAMPLES_LUA_HEADER_PATH})
    target_include_directories(wrenexample PUBLIC ${INTSPEC_EXAMPLES_WREN_HEADER_PATH})

    target_link_libraries(luaexample introspective ${LUA_LIBRARY})
    target_link_libraries(wrenexample introspective ${WREN_LIBRARY})
endif()

