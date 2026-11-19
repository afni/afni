FROM afni/afni_dev_base AS builder
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
# Use AFNI's current platform configuration, including the bundled f2c header.
USER root
RUN apt-get remove -y libf2c2-dev
USER $CONTAINER_UID
ARG AFNI_MAKEFILE_SUFFIX=linux_ubuntu_24_64
ENV CCACHE_DIR=/opt/compiler-cache CCACHE_COMPILERCHECK=content CCACHE_MAXSIZE=1G
COPY --chown=$CONTAINER_UID:$CONTAINER_GID . $AFNI_ROOT/src/
WORKDIR $AFNI_ROOT/src/src
# The mount retains compiler results after a failed RUN on the same builder.
# A normal directory below exports successful results to the next CircleCI job.
RUN --mount=type=cache,id=afni-make-compiler-cache,target=/opt/compiler-cache,uid=1000,gid=100 \
    mkdir -p "$CCACHE_DIR" /opt/ccache /opt/build-reports \
    && if test -d "$AFNI_ROOT/src/.ci-cache/ccache"; then \
         cp -a "$AFNI_ROOT/src/.ci-cache/ccache/." "$CCACHE_DIR/"; \
       fi \
    && if test -f "$AFNI_ROOT/src/.ci-cache/changes.json"; then \
         cp "$AFNI_ROOT/src/.ci-cache/changes.json" /opt/build-reports/changes.json; \
       fi \
    && rm -rf "$AFNI_ROOT/src/.ci-cache" \
    && make -f "Makefile.$AFNI_MAKEFILE_SUFFIX" afni_src.tgz \
    && python ../.circleci/check-build.py archive --source "$AFNI_ROOT/src" \
         --archive afni_src.tgz --reports /opt/build-reports \
    && tar -xzf afni_src.tgz -C "$AFNI_ROOT/build" --strip-components=1 \
    && rm afni_src.tgz \
    && cd "$AFNI_ROOT/build" \
    && cp "Makefile.$AFNI_MAKEFILE_SUFFIX" Makefile \
    && make cleanest \
    # Some legacy object rules omit these generated-header dependencies.
    && make AFNI_version.h coxplot.h niml.h \
    # Recursive library/program recipes move groups of objects and invoke
    # clean alongside build goals. Keep Make serial until that graph is fixed;
    # CI jobs and pytest shards still run concurrently.
    && make -j1 CEXE='ccache /usr/bin/gcc' all 2>&1 | tee /opt/build-reports/make.log \
    && make CEXE='ccache /usr/bin/gcc' plugins 2>&1 | tee -a /opt/build-reports/make.log \
    && make CEXE='ccache /usr/bin/gcc' install install_plugins install_lib 2>&1 | tee -a /opt/build-reports/make.log \
    # The base owns DESTDIR; copy contents without changing its metadata.
    && cp -RP "$AFNI_MAKEFILE_SUFFIX/." "$DESTDIR/" \
    && ccache --show-stats | tee /opt/build-reports/ccache.txt \
    && cp -a "$CCACHE_DIR/." /opt/ccache/ \
    && fix-permissions "$DESTDIR"
ENV PATH=$DESTDIR:$PATH

FROM afni/afni_dev_base
ENV PATH=$DESTDIR:$PATH
COPY --from=builder --chown=$CONTAINER_UID:$CONTAINER_GID /opt/afni/install /opt/afni/install
COPY --from=builder --chown=$CONTAINER_UID:$CONTAINER_GID /opt/afni/src /opt/afni/src
USER root
RUN echo "PATH=$PATH" >> /etc/environment
USER $CONTAINER_UID
WORKDIR /home/afni_user/work
