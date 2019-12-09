# FROM neurodebian:nd18.04@sha256:3b3f09ca5387f479f144a2e45fb191afa9c9f7c1bd0f03ac90941834a4e5a616
FROM thewtex/opengl:ubuntu1804@sha256:b9de45d4f594b57136f7ec3b890567ecea1421278ee4c7be80e11888bf8d23ba

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn
RUN TZ=Europe/Minsk \
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# The gpg key import is a little flaky...
COPY .docker/neurodebian.gpg /usr/local/etc/neurodebian.gpg
RUN wget  -O- http://neuro.debian.net/lists/bionic.us-nh.full > /etc/apt/sources.list.d/neurodebian.sources.list && \
    apt-key add /usr/local/etc/neurodebian.gpg && \
    (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

RUN TZ=Europe/Minsk \
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Prepare environment
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    build-essential \
    bzip2 \
    ca-certificates \
    curl \
    cython3 \
    freeglut3-dev \
    g++ \
    gcc \
    gdb \
    git \
    git-annex-standalone \
    libglew-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libglw1-mesa-dev \
    libgsl-dev \
    libgts-dev \
    libjpeg62 \
    libmotif-dev \
    libnetcdf-dev \
    libqhull-dev \
    libtool \
    libxi-dev \
    libxmhtml-dev \
    libxmu-dev \
    libxpm-dev \
    libxt-dev \
    libvolpack1-dev \
    m4 \
    ncurses-dev \
    ninja-build \
    pkg-config \
    python-dev \
    python-matplotlib \
    python-mpltoolkits.basemap \
    python-numpy \
    python-qt4 \
    python-rpy2 \
    python-scipy \
    python-tk \
    python-wxgtk3.0 \
    python3.6-dev \
    r-base \
    rsync \
    tcsh \
    tree \
    valgrind \
    vim \
    wget \
    xvfb \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

ENV CMAKE_VER=cmake-3.13.0-Linux-x86_64
RUN wget -P /cmake  https://github.com/Kitware/CMake/releases/download/v3.13.0/${CMAKE_VER}.tar.gz \
  ; cd /cmake \
  ; tar xzvf ${CMAKE_VER}.tar.gz \
  ;rm -fr ${CMAKE_VER}.tar.gz 
ENV PATH="/cmake/${CMAKE_VER}/bin:$PATH"


RUN \
 curl -fsSL https://bootstrap.pypa.io/get-pip.py | python3 - --no-cache-dir \
  && pip3 install --no-cache-dir \
        autopep8 \
        black \
        codecov \
        datalad \
        nibabel \
        numpy \
        pandas \
        pdbpp \
        pytest \
        pytest-cov \
        pytest-parallel


ENV AFNI_ROOT=/opt/afni
ENV INSTALL_DIR=/opt/abin

# Copy AFNI source code. This will likely invalidate the build cache.
RUN apt-get update && \
    eatmydata apt-get install -y --no-install-recommends \
    mesa-utils

COPY . $AFNI_ROOT/

RUN  mkdir -p /build
WORKDIR /build

RUN  cmake \
    -GNinja \
    -DBUILD_BINARIES=ON \
    -DBUILD_X_DEPENDENT_GUI_PROGS=ON \
    -DBUILD_OPENGL_DEPENDENT_GUI_PROGS=ON \
    -DBUILD_PLUGINS=ON \
    -DUSE_OMP=ON \
    -DUSE_SYSTEM_GLW=OFF \
    -DUSE_SYSTEM_XMHTML=OFF \
    $AFNI_ROOT

ENV PATH=/build/src:/opt/afni/src/scripts_install:/opt/afni/src/python_scripts/afni_python:/opt/afni/src/R_scripts:/opt/afni/src/jzosky:/opt/afni/src/jzosky/lib_RetroTS:$PATH
RUN /bin/bash -c 'set -o pipefail; ninja -v | tee -a verbose_build.log'

# RUN apsearch -update_all_afni_help
