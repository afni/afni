FROM ubuntu:bionic
# FROM ubuntu:eoan

RUN apt-get update && apt-get install -y -q eatmydata \
    && eatmydata apt-get update && apt-get install -y -q  \
        cmake \
        gcc \
        git \
        help2man \
        libexpat1-dev \
        make \
        wget \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install newer cmake version with globally scoped targets for convenience
# (apt installed version is used by default though)
ENV CMAKE_VER=3.13.3
RUN wget -P /opt/cmake  https://github.com/Kitware/CMake/releases/download/v${CMAKE_VER}/cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ; cd /opt/cmake \
  ; tar xzvf cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ;rm -fr cmake-${CMAKE_VER}-Linux-x86_64.tar.gz

RUN mkdir -p /opt/src/nifti_clib
RUN mkdir /nifti_build
COPY . /opt/src/nifti_clib/

WORKDIR /nifti_build

# Test build and install with system-installed cmake
RUN cmake \
    -DBUILD_SHARED_LIBS=ON \
    -DNIFTI_USE_PACKAGING=ON \
    -DUSE_CIFTI_CODE=ON \
    -DUSE_FSL_CODE=ON \
    -DNIFTI_BUILD_APPLICATIONS=ON \
    -DTEST_INSTALL=ON \
    -DDOWNLOAD_TEST_DATA=OFF \
    /opt/src/nifti_clib \
    && make install \
    && ctest --output-on-failure -LE NEEDS_DATA

# For checking max-verified cmake version successfully builds:
# ENV PATH="/opt/cmake/cmake-${CMAKE_VER}-Linux-x86_64/bin:$PATH"
# RUN cmake \
#     -DBUILD_SHARED_LIBS=OFF \
#     -DUSE_CIFTI_CODE=ON \
#     -DUSE_FSL_CODE=ON \
#     -DNIFTI_BUILD_APPLICATIONS=ON \
#     -DTEST_INSTALL=ON \
#     /opt/src/nifti_clib \
#     && make install \
#     && ctest --output-on-failure