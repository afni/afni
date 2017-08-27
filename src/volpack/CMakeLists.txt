cmake_minimum_required(VERSION 2.6)
project(volpack)

file(GLOB CSRC "*.c")

add_library(volpack ${CSRC})
