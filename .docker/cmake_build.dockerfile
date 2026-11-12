FROM afni/afni_dev_base AS builder
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG AFNI_WITH_COVERAGE=0
ARG BUILD_JOBS=2
ENV CCACHE_DIR=/opt/ccache CCACHE_COMPILERCHECK=content CCACHE_MAXSIZE=1G
COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/src/
WORKDIR $AFNI_ROOT/build
RUN mkdir -p "$CCACHE_DIR" /opt/build-reports \
    && if test -d "$AFNI_ROOT/src/.ci-cache/ccache"; then \
         cp -a "$AFNI_ROOT/src/.ci-cache/ccache/." "$CCACHE_DIR/"; \
       fi \
    && if test -f "$AFNI_ROOT/src/.ci-cache/changes.json"; then \
         cp "$AFNI_ROOT/src/.ci-cache/changes.json" /opt/build-reports/changes.json; \
       fi \
    && rm -rf "$AFNI_ROOT/src/.ci-cache" \
    && mkdir -p .cmake/api/v1/query && touch .cmake/api/v1/query/codemodel-v2 \
    && export CC=gcc CXX=g++ \
    && if test "$AFNI_WITH_COVERAGE" != 0; then \
         export CFLAGS='-g -O0 --coverage' CXXFLAGS='-g -O0 --coverage' LDFLAGS=--coverage; \
       fi \
    && cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
         -DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
         -DSTANDARD_PYTHON_INSTALL=OFF \
         -DUSE_SYSTEM_NIFTI=OFF -DUSE_SYSTEM_GIFTI=OFF \
         -DCOMP_GUI=ON -DCOMP_PLUGINS=ON -DUSE_OMP=ON "$AFNI_ROOT/src" \
    && cmake --build . --parallel "$BUILD_JOBS" 2>&1 | tee /opt/build-reports/cmake.log \
    && cmake --install . \
    && fix-permissions "$DESTDIR" \
    && ccache --show-stats | tee /opt/build-reports/ccache.txt
# Preserve CMake-image Python imports without copying the whole builder venv.
RUN ln -s "$DESTDIR/usr/local/bin/afnipy" "$(python -c 'import site; print(site.getsitepackages()[0])')/afnipy"
ENV PATH=$DESTDIR/usr/local/bin:$PATH

# Only install/source artifacts enter the test image, not objects or ccache.
FROM afni/afni_dev_base
ENV PATH=$DESTDIR/usr/local/bin:$PATH
COPY --from=builder --chown=$CONTAINER_UID:$CONTAINER_GID /opt/afni/install /opt/afni/install
COPY --from=builder --chown=$CONTAINER_UID:$CONTAINER_GID /opt/afni/src /opt/afni/src
# Preserve CMake-image Python imports without copying the whole builder venv.
RUN ln -s "$DESTDIR/usr/local/bin/afnipy" "$(python -c 'import site; print(site.getsitepackages()[0])')/afnipy"
USER root
RUN echo "PATH=$PATH" >> /etc/environment
USER $CONTAINER_UID
WORKDIR /home/afni_user/work
