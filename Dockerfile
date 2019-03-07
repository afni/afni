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

RUN wget https://repo.continuum.io/miniconda/Miniconda3-4.5.11-Linux-x86_64.sh && \
    bash Miniconda3-4.5.11-Linux-x86_64.sh -b -p /usr/local/miniconda && \
    rm Miniconda3-4.5.11-Linux-x86_64.sh; conda config --add channels conda-forge

ENV PATH="/usr/local/miniconda/bin:$PATH" \
    CPATH="/usr/local/miniconda/include/:$CPATH" \
    LANG="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    PYTHONNOUSERSITE=1
# conda install -c conda-forge awscli 
RUN conda install conda-build

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
RUN  mkdir -p /build
WORKDIR /build

RUN  cmake -GNinja $AFNI_ROOT
RUN ninja 
# RUN ninja install
# RUN apsearch -update_all_afni_help

