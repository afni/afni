FROM afni/afni_dev_base

# Remove f2c dev header to ensure within repo f2c.h is used
RUN apt-get remove -y libf2c2-dev


# set non interactive backend for matplotlib
RUN mkdir -p /root/.config/matplotlib \
    && echo "backend: Agg" > /root/.config/matplotlib/matplotlibrc

# Copy AFNI source code. This can invalidate the build cache.
ARG AFNI_ROOT=/opt/afni
COPY [".", "$AFNI_ROOT/"]

ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64
ARG AFNI_WITH_COVERAGE="0"
ENV PATH="/opt/abin:$PATH"

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
    && make cleanest \
    && /bin/bash -c \
    'make itall 2>&1 | tee build_log.txt && test ${PIPESTATUS[0]} -eq 0' \
    && mv $AFNI_MAKEFILE_SUFFIX /opt/abin





WORKDIR "$AFNI_ROOT"