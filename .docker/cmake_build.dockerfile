FROM afni/afni_dev_base

ENV PATH=$DESTDIR/usr/local/bin:$PATH

# Env variable, available only at build time of docker image to enable a
# coverage build. Should be set to true or false
ARG AFNI_WITH_COVERAGE="0"

# Copy AFNI source code. This will likely invalidate the build cache.
COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/

WORKDIR $AFNI_ROOT/../build

RUN \
    export CC=`which gcc`;\
    if [[ "$AFNI_WITH_COVERAGE" != "0" ]];then\
        export CXXFLAGS="-g -O0 -Wall -W -Wshadow -Wunused-variable -Wunused-parameter -Wunused-function -Wunused -Wno-system-headers -Wno-deprecated -Woverloaded-virtual -Wwrite-strings -fprofile-arcs -ftest-coverage"; \
        export CFLAGS="-g -O0 -Wall -W -fprofile-arcs -ftest-coverage"; \
        export LDFLAGS="-fprofile-arcs -ftest-coverage";\
    fi; \
    cmake \
        -GNinja \
        -DCOMP_BINARIES=ON \
        -DUSE_SYSTEM_NIFTI=OFF \
        -DUSE_SYSTEM_GIFTI=OFF \
        -DCOMP_GUI=ON \
        -DCOMP_PLUGINS=ON \
        -DUSE_OMP=ON \
        $AFNI_ROOT

RUN /bin/bash -oc pipefail \
'ninja -v 2>&1 | tee verbose_build.log && test ${PIPESTATUS[0]} -eq 0'

# install provided it is not a coverage build (which is much larger and so not installed)
RUN if [[ "$AFNI_WITH_COVERAGE" == "0" ]];then ninja install && fix-permissions $DESTDIR;fi

# For any variables that should be present for all users of the container they
# should be set in /etc/environment (variables set by ENV do not cleanly
# propagate to all users'
USER root
RUN bash -c 'echo PATH=${PATH} >> /etc/environment'
USER $CONTAINER_UID

WORKDIR /home/afni_user/work
# RUN apsearch -update_all_afni_help
