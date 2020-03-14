# This file must be used when configuring cmake for a build on osx with openmp support.
# It is used by calling cmake with the additional option
# "-DCMAKE_TOOLCHAIN_FILE=path/to/this/file". The clang packaged with Xcode does not
# have openmp support. Instead install clang via llvm: "brew install llvm"
set(CMAKE_C_COMPILER /usr/local/opt/llvm/bin/clang)
set(CMAKE_EXE_LINKER_FLAGS "-L/usr/local/opt/llvm/lib")

# Alternatively one can use gcc (brew install gcc)
# set(CMAKE_C_COMPILER /usr/local/bin/gcc-9)
# set(CMAKE_EXE_LINKER_FLAGS "-L/usr/local/opt/gcc/lib")
