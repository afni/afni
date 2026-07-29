# macOS arm64 CMake notes

The macOS arm64 build has three separate concerns that should stay separate:

1. Toolchain selection
2. Dependency discovery
3. Target-specific compile and link settings

Compiler, C++ compiler, SDK, architecture, archiver, linker, and deployment
target are toolchain concerns.  Set them in a CMake toolchain file or with
standard CMake cache variables before `project()` runs.  The arm64 example
toolchain files in this directory set the compiler pair, active macOS SDK,
`arm64` architecture, Apple command-line tools, and a default deployment target
based on the active SDK major version.  That default avoids mixing current
Homebrew dylibs with an older deployment target such as one inherited from the
compiler, environment, or command line:

```sh
cmake -S . -B build-gcc13-arm64 -G Ninja \
  -DCMAKE_TOOLCHAIN_FILE=cmake/macos_homebrew_gcc13_arm64_toolchain.cmake \
  -DAFNI_HOMEBREW_GCC_VERSION=13 \
  -DAFNI_MESA_ROOT=/opt/homebrew/opt/mesa \
  -DAFNI_GLU_ROOT=/opt/homebrew/opt/mesa-glu \
  -DCOMP_GUI=ON \
  -DCOMP_SUMA=ON \
  -DCOMP_PLUGINS=ON \
  -DCOMP_COREBINARIES=ON \
  -DCOMP_PYTHON=OFF
```

The GCC toolchain defaults to `AFNI_HOMEBREW_GCC_VERSION=13`, but the same
file can select another Homebrew GCC installation, for example
`-DAFNI_HOMEBREW_GCC_VERSION=15`.

Other compiler selections use the same component options:

```sh
# Homebrew GCC 15
cmake -S . -B build-gcc15-arm64 -G Ninja \
  -DCMAKE_TOOLCHAIN_FILE=cmake/macos_homebrew_gcc13_arm64_toolchain.cmake \
  -DAFNI_HOMEBREW_GCC_VERSION=15 \
  -DCOMP_GUI=ON \
  -DCOMP_SUMA=ON \
  -DCOMP_PLUGINS=ON \
  -DCOMP_COREBINARIES=ON \
  -DCOMP_PYTHON=OFF

# Homebrew LLVM clang
cmake -S . -B build-llvm-arm64 -G Ninja \
  -DCMAKE_TOOLCHAIN_FILE=cmake/macos_homebrew_llvm_arm64_toolchain.cmake \
  -DCOMP_GUI=ON \
  -DCOMP_SUMA=ON \
  -DCOMP_PLUGINS=ON \
  -DCOMP_COREBINARIES=ON \
  -DCOMP_PYTHON=OFF

# AppleClang from Xcode Command Line Tools
SDK_MAJOR="$(xcrun --show-sdk-version | sed 's/\..*/.0/')"
CC=/usr/bin/clang CXX=/usr/bin/clang++ \
cmake -S . -B build-appleclang-arm64 -G Ninja \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="${SDK_MAJOR}" \
  -DCOMP_GUI=ON \
  -DCOMP_SUMA=ON \
  -DCOMP_PLUGINS=ON \
  -DCOMP_COREBINARIES=ON \
  -DCOMP_PYTHON=OFF
```

On native Apple Silicon, CMake normally selects `arm64` for the AppleClang
command.  Add `-DCMAKE_OSX_ARCHITECTURES=arm64` when running under Rosetta,
cross-compiling, or scripting a fixed arm64 build.  Build any configured tree
with:

```sh
cmake --build <build-dir> -j$(sysctl -n hw.logicalcpu)
```

The GCC toolchain file checks the compiler's configured SDK automatically.  To
inspect the same inputs by hand:

```sh
/opt/homebrew/bin/gcc-13 -v 2>&1 | sed -n 's/.*--with-sysroot=\([^ ]*\).*/\1/p'
xcrun --show-sdk-path
xcrun --show-sdk-version
```

If Homebrew GCC was configured against a newer SDK major version than the active
Command Line Tools SDK, the toolchain fails early with an explanation instead of
letting compiler detection fail later with an opaque linker error such as
`library not found for -lSystem`.

If a distributable build needs an older minimum macOS version, set
`-DCMAKE_OSX_DEPLOYMENT_TARGET=<version>` explicitly and make sure every linked
Homebrew library was built for that version or older.

Dependency discovery happens after `project()`, when CMake knows the selected
compiler and platform.  `afni_macos_dependency_hints.cmake` adds Homebrew
prefixes to `CMAKE_PREFIX_PATH`, auto-fills `AFNI_MESA_ROOT` and
`AFNI_GLU_ROOT` from `brew --prefix` for Apple Silicon SUMA builds, and adds
Homebrew `libomp` hints only for AppleClang when the user has not already set
OpenMP cache variables.

Homebrew LLVM clang can use the OpenMP runtime shipped with the LLVM formula.
AppleClang needs the separate Homebrew `libomp` formula for `USE_OMP=ON`; if it
is not installed, the build can still configure and compile with `USE_OMP=OFF`.

SUMA on macOS still uses XQuartz for X11 and GLUT.  `AFNI_MESA_ROOT` only
redirects GL to Mesa, and `AFNI_GLU_ROOT` can redirect GLU to `mesa-glu`.
`FindXQuartzGL.cmake` owns that split so SUMA targets can keep linking to the
normal `XQuartzGL::GL`, `XQuartzGL::GLU`, and `XQuartzGL::GLUT` targets.

When Homebrew dylibs are found, the build checks each inspected library's
minimum macOS version against `CMAKE_OSX_DEPLOYMENT_TARGET`.  The versions do
not have to be textually identical: a library built with SDK 26.4 can be used
with SDK 26.5.  What matters is that the deployment target is not older than
the library's recorded minimum OS version.

On native Apple Silicon terminals, CMake normally selects `arm64` without an
extra architecture flag.  The post-`project()` dependency hints detect that
selected architecture from CMake and do not change it.  The arm64 toolchain
files still set `CMAKE_OSX_ARCHITECTURES=arm64` as a default so Rosetta,
cross-compiling, and scripted builds do not depend on the invoking process
tree's architecture.  An Intel macOS toolchain should be a separate file with
`CMAKE_OSX_ARCHITECTURES=x86_64` and an Intel Homebrew prefix, typically
`/usr/local`.

## Distribution

The Homebrew-oriented configuration above is a local developer build path, not
by itself a standalone binary distribution contract.  A binary that dynamically
links to Homebrew libraries should not assume users can later install an exact
matching set of Homebrew formula versions, because Homebrew is a rolling package
manager.

The strategy closest to the existing AFNI binary distribution model is a
standalone AFNI tree:

1. Install AFNI into a relocatable directory containing `bin`, `lib`, and data
   directories.
2. Bundle non-system dylibs that AFNI expects at runtime.  This includes
   Homebrew-provided dependencies such as Motif, JPEG, GSL, Mesa GL, and
   `mesa-glu`, plus any transitive non-system dylibs they require.
3. Use the CMake install tree as the starting point.  The build already computes
   a relative install RPATH from installed `bin` to installed `lib`; on macOS
   this is `@loader_path/../lib` for the default layout.  CMake's generated
   install scripts also remove the build-tree RPATH from installed targets.
4. After copying external dylibs into the AFNI tree, rewrite their install names
   and dependency references with `install_name_tool` so AFNI binaries and
   bundled dylibs resolve them through `@loader_path` or `@rpath`, not through
   `/opt/homebrew`, `/usr/local`, or a developer's build prefix.  This step is
   still needed for copied Homebrew dylibs because CMake only controls targets it
   builds and installs itself.
5. Audit the result with `otool -L` from a clean machine or VM.  Runtime links
   should resolve to system libraries, XQuartz libraries where intentionally
   external, or libraries inside the AFNI tree.
6. Choose `CMAKE_OSX_DEPLOYMENT_TARGET` as the minimum macOS version the package
   intends to support, then make sure every bundled dylib records a minimum
   macOS version no newer than that target.  Building the package and its bundled
   dependencies on the oldest supported macOS release is one practical way to
   keep that floor low.  This is not a plan to link against users' old or new
   Homebrew libraries at runtime; the point is to produce a self-contained set
   of binaries whose recorded macOS requirements are compatible with the support
   target.

Less likely, but possible, is packaging AFNI as a Homebrew formula and letting
Homebrew manage the dependency graph.  That is a different distribution model
from AFNI's current standalone binary approach.

XQuartz should remain an external prerequisite for SUMA/X11 use.  It provides
the X server/runtime environment, not only linkable libraries.
