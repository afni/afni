FROM neurodebian:nd18.04@sha256:3b3f09ca5387f479f144a2e45fb191afa9c9f7c1bd0f03ac90941834a4e5a616
# FROM thewtex/opengl:ubuntu1804@sha256:b9de45d4f594b57136f7ec3b890567ecea1421278ee4c7be80e11888bf8d23ba

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn
RUN TZ=Europe/Minsk \
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone


RUN apt-get update && apt-get install -y wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 
# The gpg key import is a little flaky...
# COPY .docker/neurodebian.gpg /usr/local/etc/neurodebian.gpg
# RUN wget  -O- http://neuro.debian.net/lists/bionic.us-nh.full > /etc/apt/sources.list.d/neurodebian.sources.list && \
#     apt-key add /usr/local/etc/neurodebian.gpg && \
#     (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

RUN TZ=Europe/Minsk \
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install runtime and basic dependencies
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    freeglut3-dev \
    git \
    libf2c2-dev \
    libglew-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libglw1-mesa-dev \
    libgsl-dev \
    libgts-dev \
    libjpeg62 \
    libmotif-dev \
    libnetcdf-dev \
    libxi-dev \
    libxmhtml-dev \
    libxmu-dev \
    libxpm-dev \
    libxt-dev \
    libvolpack1-dev \
    python-dev \
    python-qt4 \
    python-rpy2 \
    python-scipy \
    python-tk \
    python-wxgtk3.0 \
    python3.6-dev \
    qhull-bin \
    r-base \
    tcsh \
    xvfb \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

# Install development dependencies
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    build-essential \
    bzip2 \
    cython3 \
    f2c \
    g++ \
    gcc \
    git-annex-standalone \
    libtool \
    m4 \
    ncurses-dev \
    ninja-build \
    pkg-config \
    python-matplotlib \
    python-mpltoolkits.basemap \
    python-numpy \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

# Install test dependencies and some useful tools
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    gdb \
    rsync \
    tree \
    valgrind \
    vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 


ENV CMAKE_VER=3.14.7
RUN wget -P /cmake  https://github.com/Kitware/CMake/releases/download/v${CMAKE_VER}/cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ; cd /cmake \
  ; tar xzvf cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ;rm -fr cmake-${CMAKE_VER}-Linux-x86_64.tar.gz 
ENV PATH="/cmake/cmake-${CMAKE_VER}-Linux-x86_64/bin:$PATH"

# Add some more test dependencies
RUN \
 curl -fsSL https://bootstrap.pypa.io/get-pip.py | python3 - --no-cache-dir \
  && pip3 install --no-cache-dir \
        autopep8 \
        black \
        codecov \
        datalad \
        matplotlib \
        nibabel \
        numpy \
        pandas \
        pdbpp \
        pytest \
        pytest-cov \
        pytest-parallel
