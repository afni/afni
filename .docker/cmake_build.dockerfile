FROM afni/afni_dev_base

ENV PATH=$DESTDIR/usr/local/bin:$PATH
ARG AFNI_WITH_COVERAGE="0"

# Copy AFNI source code. This will likely invalidate the build cache.
USER root
COPY . $AFNI_ROOT/
RUN fix-permissions $AFNI_ROOT
USER $CONTAINER_UID
# Will work for docker version > 19.0.3 and drop the image size substantially
# COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/

WORKDIR $AFNI_ROOT/../build

RUN  cmake \
    -GNinja \
    -DCOMP_ADD_BINARIES=ON \
    -DUSE_SYSTEM_NIFTI=OFF \
    -DUSE_SYSTEM_GIFTI=OFF \
    -DCOMP_X_DEPENDENT_GUI_PROGS=ON \
    -DCOMP_ADD_PLUGINS=ON \
    -DUSE_OMP=ON \
    -DUSE_PIP=ON \
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
