# FROM jupyter/base-notebook
# USER root
# ENV DEBIAN_FRONTEND=noninteractive
FROM neurodebian:xenial
# Prepare environment
RUN apt-get update && \
    eatmydata apt-get install -y --no-install-recommends \
                    curl \
                    bzip2 \
                    ca-certificates \
                    xvfb \
                    cython3 \
                    build-essential \
                    autoconf \
                    libtool \
                    ncurses-dev \
                    vim \
                    git \
                    libmotif-dev \
                    libnetcdf-dev \
                    tcsh \
                    r-base \
                    python-qt4 \
                    wget \
                    pkg-config \
                    && apt-get clean \
                    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

ENV CMAKE_VER=cmake-3.13.0-Linux-x86_64
RUN wget -P /cmake  https://github.com/Kitware/CMake/releases/download/v3.13.0/${CMAKE_VER}.tar.gz \
  ; cd /cmake \
  ; tar xzvf ${CMAKE_VER}.tar.gz \
  ;rm -fr ${CMAKE_VER}.tar.gz 
ENV PATH="/cmake/${CMAKE_VER}/bin:$PATH"




RUN apt-get update && \
    eatmydata apt-get install -y --no-install-recommends \
    freeglut3-dev \
    libgsl-dev \
    libglew-dev \
    libglib2.0-dev \
    libglw-dev \
    libinsighttoolkit3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

RUN apt-get update && \
    eatmydata apt-get install -y --no-install-recommends \
    libxmu-dev \
    libxpm-dev \
    libxt-dev  \
    libjpeg62 \
    python-matplotlib \
    python-rpy2 \
    python-tk \
    python-numpy \
    ninja-build


RUN \
    if [ "$AFNI_WITH_COVERAGE" != "0" ]; then \
      echo "Adding testing and coverage components" \
      && curl -fsSL https://bootstrap.pypa.io/get-pip.py | python3 - --no-cache-dir \
      && pip3 install --no-cache-dir \
            codecov \
            pytest \
            pytest-cov \
            numpy \
            pandas; \
    fi 


RUN mkdir /gbuild;cd /gbuild \
&& git clone https://github.com/leej3/gifti_clib.git /gifti_src\
&& cd /gifti_src \
&& git checkout v0.0.11 \
&& cd /gbuild \
&& cmake -GNinja -DBUILD_SHARED_LIBS:BOOL=ON /gifti_src \
&& ninja install

ENV AFNI_ROOT=/afni
# Copy AFNI source code. This can invalidate the build cache.
ADD . $AFNI_ROOT/
ADD cmake $AFNI_ROOT/cmake/
ADD CMakeLists.txt $AFNI_ROOT/
ADD Dockerfile $AFNI_ROOT/
RUN  mkdir -p /build
WORKDIR /build

RUN  cmake -GNinja $AFNI_ROOT
RUN ninja 
# RUN apsearch -update_all_afni_help

