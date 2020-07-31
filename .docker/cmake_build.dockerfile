FROM afni/afni_dev_base

ENV PATH=$DESTDIR/usr/local/bin:$PATH

# Env variable, available only at build time of docker image to enable a
# coverage build. Anything but a value of 0 will cause coverage to be used.
ARG AFNI_WITH_COVERAGE="0"

# Copy AFNI source code. This will likely invalidate the build cache.
USER root
COPY . $AFNI_ROOT/
RUN fix-permissions $AFNI_ROOT
USER $CONTAINER_UID
# Will work for docker version > 19.0.3 and drop the image size substantially
# COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/

WORKDIR $AFNI_ROOT/../build

RUN \
    export CC=`which gcc`;\
    if [ "$AFNI_WITH_COVERAGE" != "0" ];then\
        export CXXFLAGS="-g -O0 -Wall -W -Wshadow -Wunused-variable -Wunused-parameter -Wunused-function -Wunused -Wno-system-headers -Wno-deprecated -Woverloaded-virtual -Wwrite-strings -fprofile-arcs -ftest-coverage"; \
        export CFLAGS="-g -O0 -Wall -W -fprofile-arcs -ftest-coverage"; \
        export LDFLAGS="-fprofile-arcs -ftest-coverage";\
    fi; \
    cmake \
        -GNinja \
        -DCOMP_ADD_BINARIES=ON \
        -DUSE_SYSTEM_NIFTI=OFF \
        -DUSE_SYSTEM_GIFTI=OFF \
        -DCOMP_X_DEPENDENT_GUI_PROGS=ON \
        -DCOMP_ADD_PLUGINS=ON \
        -DUSE_OMP=ON \
        $AFNI_ROOT

RUN /bin/bash -oc pipefail \
'ninja -v 2>&1 | tee verbose_build.log && test ${PIPESTATUS[0]} -eq 0'

RUN ninja install && fix-permissions $DESTDIR

# For any variables that should be present for all users of the container they
# should be set in /etc/environment (variables set by ENV do not cleanly
# propagate to all users'
USER root
RUN bash -c 'echo PATH=${PATH} >> /etc/environment'
USER $CONTAINER_UID

WORKDIR /home/afni_user/work
# RUN apsearch -update_all_afni_help
