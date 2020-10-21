FROM afni/afni_dev_base

# Remove f2c dev header to ensure within repo f2c.h is used
USER root
RUN apt-get remove -y libf2c2-dev

ENV DESTDIR=$AFNI_ROOT/../install
ENV PATH=$DESTDIR:$PATH

# Copy AFNI source code. This will likely invalidate the build cache.
COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/

# Not supported, try the cmake build for coverage testing
ENV AFNI_WITH_COVERAGE=false

ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64_glw_local_shared
ARG KEEP_BUILD_DIR="0"
RUN cd $AFNI_ROOT/src \
    && make -f  other_builds/Makefile.$AFNI_MAKEFILE_SUFFIX afni_src.tgz \
    && tar -xzf afni_src.tgz -C $AFNI_ROOT/../build --strip-components=1 \
    && rm afni_src.tgz \
    # copy and possibly modify makefile
    && cd $AFNI_ROOT/../build \
    && cp other_builds/Makefile.$AFNI_MAKEFILE_SUFFIX Makefile \
    # clean and move source code to build directory
    && make cleanest \
    # Build AFNI.
    && /bin/bash -c \
    'make itall 2>&1 | tee build_log.txt && test ${PIPESTATUS[0]} -eq 0' \
    && mv $AFNI_MAKEFILE_SUFFIX/* $AFNI_ROOT/../install \
    # Remove build tree to drop image size
    && if [ "$KEEP_BUILD_DIR" = "0" ]; then \
      rm -rf $AFNI_ROOT/../build; \
      fi

USER root
RUN bash -c 'echo PATH=${PATH} >> /etc/environment'
USER $CONTAINER_UID

WORKDIR $HOME/work
