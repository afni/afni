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
RUN cd $AFNI_ROOT/src \
    # copy and possibly modify makefile
    && cp other_builds/Makefile.$AFNI_MAKEFILE_SUFFIX Makefile \
    # clean and build directly from the checked-out source, rather than
    # repackaging it through the afni_src.tgz release-distribution target
    # first -- that target is a hand-maintained file/directory manifest
    # that has repeatedly drifted out of sync with new source directories
    && make cleanest \
    # Build AFNI.
    && /bin/bash -c \
    'make itall 2>&1 | tee build_log.txt && test ${PIPESTATUS[0]} -eq 0' \
    && mv $AFNI_MAKEFILE_SUFFIX/* $AFNI_ROOT/../install

USER root
RUN bash -c 'echo PATH=${PATH} >> /etc/environment'
USER $CONTAINER_UID

WORKDIR $HOME/work
