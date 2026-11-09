FROM neurodebian:nd24.04@sha256:b884da86b9c76b64c9d9a11d1b289391f21fd2f73405a6be1d433079e885e1b5

ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update && apt-get install -y --no-install-recommends \
    bc build-essential bzip2 ca-certificates ccache curl f2c freeglut3-dev \
    gdb git git-annex libexpat1-dev libf2c2-dev libglew-dev libglib2.0-dev \
    libglu1-mesa-dev libglw1-mesa-dev libgsl-dev libgts-dev libjpeg-dev \
    libjpeg-turbo-progs libmotif-dev libncurses-dev libtool libxi-dev \
    libxmhtml-dev libxmu-dev libxpm-dev libxt-dev locales m4 netpbm \
    ninja-build pkg-config python3-dev python3-rpy2 python3-venv \
    python3-wxgtk4.0 qhull-bin r-base rsync sudo tcsh tini valgrind \
    wget x11-apps xauth xvfb zlib1g-dev \
    && rm -rf /var/lib/apt/lists/* \
    && sed -i 's/^# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && locale-gen \
    && ln -s /usr/bin/python3 /usr/bin/python \
    # Retain the original image's shell behavior for legacy AFNI scripts.
    && ln -sf /bin/bash /bin/sh

ENV SHELL=/bin/bash \
    CONTAINER_USER=afni_user \
    CONTAINER_UID=1000 \
    CONTAINER_GID=100 \
    PYTHONUSERBASE=/opt/user_pip_packages \
    VIRTUAL_ENV=/opt/venv \
    TINI_SUBREAPER=1 \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    AFNI_ROOT=/opt/afni \
    HOME=/home/afni_user \
    MPLBACKEND=Agg
ENV AFNI_ATLAS_PATH=$AFNI_ROOT/atlases \
    DESTDIR=$AFNI_ROOT/install \
    PATH=/opt/venv/bin:$PATH

COPY .docker/fix-permissions .docker/start.sh /usr/local/bin/
RUN chmod a+rx /usr/local/bin/fix-permissions /usr/local/bin/start.sh \
    && mkdir -p /usr/local/bin/image_startup.d

# Noble's parent image may already reserve UID 1000 for an unused ubuntu user.
# Do not remove any other account if an upstream image changes that convention.
RUN if getent passwd "$CONTAINER_UID"; then \
      test "$(getent passwd "$CONTAINER_UID" | cut -d: -f1)" = ubuntu && userdel ubuntu; \
    fi \
    && useradd -m -s /bin/bash -u "$CONTAINER_UID" -g "$CONTAINER_GID" "$CONTAINER_USER" \
    && chmod g+w /etc/passwd \
    && mkdir -p "$HOME/work" "$PYTHONUSERBASE" "$AFNI_ROOT"/{build,src,install,atlases} \
    && fix-permissions "$HOME" /opt

# Keep distro-managed wx/rpy2 available, but isolate pip from Ubuntu's managed
# Python. The numerical/test stack is pinned separately from the OS packages.
COPY .docker/requirements-ci.txt /tmp/requirements-ci.txt
RUN python3 -m venv --system-site-packages "$VIRTUAL_ENV" \
    && python -m pip install --no-cache-dir -r /tmp/requirements-ci.txt \
    && python -m pip check \
    && fix-permissions /opt

RUN curl -fL --retry 3 https://afni.nimh.nih.gov/pub/dist/atlases/afni_atlases_dist.tgz \
        -o /tmp/afni_atlases.tgz \
    && tar -xzf /tmp/afni_atlases.tgz -C "$AFNI_ATLAS_PATH" --strip-components=1 \
    && rm /tmp/afni_atlases.tgz \
    && fix-permissions /opt

# start.sh preserves PATH when switching users; /etc/environment also supports
# callers that invoke sudo/su themselves.
RUN for var in PYTHONUSERBASE VIRTUAL_ENV AFNI_ATLAS_PATH AFNI_ROOT DESTDIR PATH TINI_SUBREAPER LC_ALL; do \
      echo "$var=${!var}" >> /etc/environment; \
    done
USER $CONTAINER_UID
RUN git config --global user.name 'AFNI CI' \
    && git config --global user.email nobody@example.com
WORKDIR /home/afni_user/work
ENTRYPOINT ["tini", "-g", "--", "start.sh"]
