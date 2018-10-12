FROM ubuntu:bionic-20180526@sha256:c8c275751219dadad8fa56b3ac41ca6cb22219ff117ca98fe82b42f24e1ba64e

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y -qq \
    && apt-get install -yq --no-install-recommends \
          ca-certificates \
          curl \
          g++ \
          gcc \
          git \
          libglib2.0-dev \
          libglu1-mesa-dev \
          libglw1-mesa-dev \
          libgsl-dev \
          libmotif-dev \
          libxi-dev \
          libxmhtml-dev \
          libxmu-dev \
          libxpm-dev \
          libxt-dev \
          m4 \
          python-dev \
          python-matplotlib \
          python-numpy \
          python-scipy \
          python-qt4 \
          python-rpy2 \
          python-tk \
          python-mpltoolkits.basemap \
          r-base \
          tcsh \
          vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
    && curl -fsSL https://bootstrap.pypa.io/get-pip.py | python - --no-cache-dir \
    && pip install --no-cache-dir \
          scipy \
          rpy2 

# Copy AFNI source code. This can invalidate the build cache.
ARG AFNI_ROOT=/opt/afni
COPY [".", "$AFNI_ROOT/"]

ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64
ARG AFNI_WITH_COVERAGE="0"

WORKDIR "$AFNI_ROOT/src"
RUN \
    if [ "$AFNI_WITH_COVERAGE" != "0" ]; then \
      echo "Adding testing and coverage components" \
      && sed -i 's/# CPROF = /CPROF =  -coverage /' Makefile.$AFNI_MAKEFILE_SUFFIX \
      && curl -fsSL https://bootstrap.pypa.io/get-pip.py | python3 - --no-cache-dir \
      && pip3 install --no-cache-dir \
            codecov \
            pytest \
            pytest-cov \
            numpy \
            pandas; \
    fi \
    \
    # Clean AFNI src directory (*.o files can cause build to fail).
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
    && make itall \
    && mv $AFNI_MAKEFILE_SUFFIX $AFNI_ROOT/abin

ENV PATH="$AFNI_ROOT/abin:$PATH"
WORKDIR "$AFNI_ROOT"
