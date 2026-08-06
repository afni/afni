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

# Empty by default so the build Makefile is chosen by architecture below.
# An explicit value still overrides (preserving the previous behavior for
# anyone passing --build-arg AFNI_MAKEFILE_SUFFIX=...).
ARG AFNI_MAKEFILE_SUFFIX=""
ARG KEEP_BUILD_DIR="0"
RUN cd $AFNI_ROOT/src \
    # Pick an architecture-appropriate build Makefile if one was not supplied.
    # linux_ubuntu_16_64_glw_local_shared is x86_64-only (it hardcodes -m64);
    # aarch64 uses linux_ubuntu_24_ARM (merged upstream, arch-clean, builds the
    # same shared libmri.so/libf2c.so layout via MRI_SHARED).
    && if [ -z "$AFNI_MAKEFILE_SUFFIX" ]; then \
         case "$(uname -m)" in \
           aarch64) AFNI_MAKEFILE_SUFFIX=linux_ubuntu_24_ARM ;; \
           *)       AFNI_MAKEFILE_SUFFIX=linux_ubuntu_16_64_glw_local_shared ;; \
         esac; \
       fi \
    # The chosen Makefile may live under other_builds/ (x86_64 container
    # variants) or at the src root (the ARM Makefile); afni_src.tgz packages
    # both, so resolve whichever exists.
    && if [ -f other_builds/Makefile.$AFNI_MAKEFILE_SUFFIX ]; then \
         AFNI_MAKEFILE=other_builds/Makefile.$AFNI_MAKEFILE_SUFFIX; \
       else \
         AFNI_MAKEFILE=Makefile.$AFNI_MAKEFILE_SUFFIX; \
       fi \
    && make -f "$AFNI_MAKEFILE" afni_src.tgz \
    && tar -xzf afni_src.tgz -C $AFNI_ROOT/../build --strip-components=1 \
    && rm afni_src.tgz \
    # copy and possibly modify makefile
    && cd $AFNI_ROOT/../build \
    && cp "$AFNI_MAKEFILE" Makefile \
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
