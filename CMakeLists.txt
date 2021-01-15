cmake_minimum_required(VERSION 3.17)
project(Virgo)

set(CMAKE_CXX_STANDARD 14)

add_executable(Virgo main.cpp virgo.h)

# test to check max stack depth limit
# target_link_libraries(Virgo PRIVATE "-Wl,--stack,10000000")