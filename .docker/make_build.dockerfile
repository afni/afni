FROM neurodebian:nd18.04@sha256:3b3f09ca5387f479f144a2e45fb191afa9c9f7c1bd0f03ac90941834a4e5a616

RUN TZ=Europe/Minsk \
ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Prepare environment
RUN apt-get update && \
    eatmydata apt-get install -y --no-install-recommends \
    build-essential \
    bzip2 \
    ca-certificates \
    curl \
    cython3 \
    freeglut3-dev \
    g++ \
    gcc \
    git \
    git-annex-standalone \
    libglew-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libglw1-mesa-dev \
    libgsl-dev \
    libjpeg62 \
    libmotif-dev \
    libnetcdf-dev \
    libtool \
    libxi-dev \
    libxmhtml-dev \
    libxmu-dev \
    libxpm-dev \
    libxt-dev \
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
    python3.6 \
    r-base \
    rsync \
    tcsh \
    vim \
    wget \
    xvfb \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

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


# Copy AFNI source code. This can invalidate the build cache.
ARG AFNI_ROOT=/opt/afni
COPY [".", "$AFNI_ROOT/"]

ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64
ARG AFNI_WITH_COVERAGE="0"

WORKDIR "$AFNI_ROOT/src"
RUN \
    if [ "$AFNI_WITH_COVERAGE" != "0" ]; then \
      echo "Adding testing and coverage components" \
      && sed -i 's/# CPROF = /CPROF =  -coverage /' Makefile.$AFNI_MAKEFILE_SUFFIX ;\
      fi \
    && make -f  Makefile.$AFNI_MAKEFILE_SUFFIX afni_src.tgz \
    && mv afni_src.tgz .. \
    && cd .. \
    \
    # Empty the src directory, and replace with the contents of afni_src.tgz
    && rm -rf src/ && mkdir src \
    && tar -xzf afni_src.tgz -C $AFNI_ROOT/src --strip-components=1 \
    && rm afni_src.tgz \
    \
    # Build AFNI.
    && cd src \
    && cp Makefile.$AFNI_MAKEFILE_SUFFIX Makefile \
    # Clean in case there are some stray object files
    && make cleanest 

RUN  /bin/bash -oc pipefail   make itall 2>1 | tee /build_log.txt 

ENV PATH="$AFNI_ROOT/src:$PATH"

# set non interactive backend for matplotlib
RUN mkdir -p /root/.config/matplotlib \
    && echo "backend: Agg" > /root/.config/matplotlib/matplotlibrc

WORKDIR "$AFNI_ROOT"