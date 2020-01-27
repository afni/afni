FROM afni/afni_dev_base

ENV AFNI_ROOT=/opt/src/afni
ENV INSTALL_DIR=/opt/abin

# Copy AFNI source code. This will likely invalidate the build cache.
COPY . $AFNI_ROOT/

RUN  mkdir -p /build
WORKDIR /build

RUN  cmake \
    -GNinja \
    -DBUILD_BINARIES=ON \
    -DUSE_SYSTEM_NIFTI=OFF \
    -DUSE_SYSTEM_GIFTI=OFF \
    -DBUILD_X_DEPENDENT_GUI_PROGS=ON \
    -DBUILD_PLUGINS=ON \
    -DUSE_OMP=ON \
    $AFNI_ROOT

RUN /bin/bash -oc pipefail \
ninja -v | tee -a verbose_build.log 2>&1

# RUN apsearch -update_all_afni_help
