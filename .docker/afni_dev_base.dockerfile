# neurodebian:nd18.04 has no linux/arm64 manifest, so the base is plain
# ubuntu:18.04 (multi-arch). The neurodebian base did carry its own apt repos
# (used implicitly for git-annex-standalone); the two consequences of dropping
# it are handled below: the git-core PPA (for a datalad-compatible git) and
# the git-annex package in place of git-annex-standalone.
FROM ubuntu:18.04

# FROM thewtex/opengl:ubuntu1804@sha256:b9de45d4f594b57136f7ec3b890567ecea1421278ee4c7be80e11888bf8d23ba

ENV DEBIAN_FRONTEND=noninteractive

ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn

RUN apt-get update && apt-get install -y wget sudo locales \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# datalad (a test dependency) requires git >= 2.19.1, but Ubuntu 18.04 ships
# 2.17.1. The former neurodebian base supplied a newer git via the
# neurodebian-only git-annex-standalone package; on the plain ubuntu base we
# pull a current git from the git-core PPA instead, which serves both amd64
# and arm64.
RUN apt-get update && apt-get install -y --no-install-recommends \
                    software-properties-common \
    && add-apt-repository -y ppa:git-core/ppa \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# The gpg key import is a little flaky...
# COPY .docker/neurodebian.gpg /usr/local/etc/neurodebian.gpg
# RUN wget -O- http://neuro.debian.net/lists/bionic.us-nh.full > /etc/apt/sources.list.d/neurodebian.sources.list && \
#     apt-key add /usr/local/etc/neurodebian.gpg && \
#     (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

# Configure environment
RUN ln -sf /bin/bash /bin/sh # use bash by default

ENV SHELL=/bin/bash \
    CONTAINER_USER="afni_user" \
    CONTAINER_UID="1000" \
    CONTAINER_GID="100" \
    PYTHONUSERBASE=/opt/user_pip_packages \
    TINI_SUBREAPER="" \
    LANG="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    AFNI_ROOT=/opt/afni/src

ENV DESTDIR="$AFNI_ROOT/../install" \
    PATH="$PYTHONUSERBASE/bin:$PATH" \
    HOME=/home/$CONTAINER_USER

# For any variables that should be present for all users of the container they
# should be set in /etc/environment (variables set by ENV do not cleanly
# propagate to all users). Should do this for PATH again later in the dockerfile (or
# child files)
ENV PRESERVED_VARS "PYTHONUSERBASE AFNI_ROOT DESTDIR PATH TINI_SUBREAPER LC_ALL"

RUN bash -c 'for val in $PRESERVED_VARS;do \
    echo $val=${!val} >> /etc/environment ; \
done'

# Copy a script that we will use to correct permissions after running certain commands
COPY .docker/fix-permissions /usr/local/bin/fix-permissions
RUN chmod a+rx /usr/local/bin/fix-permissions

# Add a lightweight init for container
RUN wget https://github.com/krallin/tini/releases/download/v0.19.0/tini-static -O /usr/local/bin/tini && chmod a+x /usr/local/bin/tini

# Enable prompt color in the skeleton .bashrc before creating the default CONTAINER_USER
RUN sed -i 's/^#force_color_prompt=yes/force_color_prompt=yes/' /etc/skel/.bashrc

RUN echo "auth requisite pam_deny.so" >> /etc/pam.d/su && \
    sed -i.bak -e 's/^%admin/#%admin/' /etc/sudoers && \
    sed -i.bak -e 's/^%sudo/#%sudo/' /etc/sudoers && \
    useradd -m -s /bin/bash -N -u $CONTAINER_UID $CONTAINER_USER && \
    chmod g+w /etc/passwd && \
    fix-permissions $HOME \
    && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && dpkg-reconfigure --frontend=noninteractive locales \
    && update-locale LANG="en_US.UTF-8"

# Install runtime and basic dependencies
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    freeglut3-dev \
    git \
    libf2c2-dev \
    libglew-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libglw1-mesa-dev \
    libgsl-dev \
    libgts-dev \
    libjpeg62-dev \
    libmotif-dev \
    libxi-dev \
    libxmhtml-dev \
    libxmu-dev \
    libxpm-dev \
    libxt-dev \
    netpbm \
    libjpeg-progs \
    python3-rpy2 \
    python3-wxgtk4.0 \
    python3.6-dev \
    qhull-bin \
    r-base \
    tcsh \
    xvfb \
    bc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install development dependencies
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    build-essential \
    bzip2 \
    f2c \
    g++ \
    gcc \
    git-annex \
    libncurses-dev \
    libtool \
    m4 \
    ninja-build \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install test dependencies and some useful tools
RUN apt-get update && apt-get install -y eatmydata && \
    eatmydata apt-get install -y --no-install-recommends \
    gdb \
    rsync \
    tree \
    valgrind \
    vim \
    x11-apps \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# set non interactive backend for matplotlib
RUN mkdir -p /root/.config/matplotlib \
    && echo "backend: Agg" > /root/.config/matplotlib/matplotlibrc

RUN ln -s /usr/bin/python3 /usr/bin/python

COPY .docker/start.sh /usr/local/bin
RUN mkdir /usr/local/bin/image_startup.d && fix-permissions /usr/local/bin/image_startup.d

RUN fix-permissions /opt

USER $CONTAINER_UID

###### Switch to non privileged user ######

RUN bash -c 'mkdir -p $AFNI_ROOT/../{build,src,install} && fix-permissions $AFNI_ROOT/../..'

# [PT: 2025-xx-xx] Bump CMake from 3.14.7 -> 3.31.7 (CMakeLists.txt requires >= 3.16).
# Note: starting with CMake 3.20, the prebuilt tarball uses lowercase "linux"
# in the filename (was "Linux" in 3.14.x and earlier).
ENV CMAKE_VER=3.31.7

# CMake publishes separate tarballs per architecture (linux-x86_64, linux-aarch64);
# resolve the right one at build time instead of hardcoding a single arch.
RUN ARCH="$(uname -m)" \
    && case "$ARCH" in \
         x86_64)  CMAKE_ARCH=linux-x86_64 ;; \
         aarch64) CMAKE_ARCH=linux-aarch64 ;; \
         *) echo "Unsupported architecture for CMake install: $ARCH" >&2; exit 1 ;; \
       esac \
    && wget -P /opt/cmake \
      https://github.com/Kitware/CMake/releases/download/v${CMAKE_VER}/cmake-${CMAKE_VER}-${CMAKE_ARCH}.tar.gz \
    && cd /opt/cmake \
    && tar xzvf cmake-${CMAKE_VER}-${CMAKE_ARCH}.tar.gz \
    && rm -fr cmake-${CMAKE_VER}-${CMAKE_ARCH}.tar.gz \
    && ln -s cmake-${CMAKE_VER}-${CMAKE_ARCH} current \
    && fix-permissions /opt

ENV PATH="/opt/cmake/current/bin:$PATH"

RUN mkdir $PYTHONUSERBASE

# Add some more test dependencies
# [PT: Feb 1, 2022] CircleCI wanted the following change, because we
# specifically use Python 3.6 (alternatively, could update the ver of Python
# below?)
RUN curl -fsSL https://bootstrap.pypa.io/pip/3.6/get-pip.py \
     | python3 - --no-cache-dir --prefix $PYTHONUSERBASE

RUN python3 -m pip install \
    --no-cache-dir \
    autopep8 \
    black==20.8b1 \
    codecov \
    cython \
    datalad \
    distro \
    docker \
    filelock \
    gcovr \
    ipython \
    matplotlib \
    nibabel \
    'numpy>=1.14.5' \
    pandas \
    pdbpp \
    pytest \
    pytest-cov \
    pytest-xdist \
    scipy \
    git+https://github.com/leej3/xvfbwrapper.git@add_support_for_xquartz_and_multi_threading \
    && fix-permissions /opt \
    && git config --global user.name "Docker Almighty" \
    && git config --global user.email "nobody@example.com" \
    && datalad wtf

# add pdb alias ipy for easier pdb debugging
RUN echo 'alias ipy from IPython import embed;embed()' >> ~/.pdbrc

RUN mkdir $HOME/work && fix-permissions $HOME/work

WORKDIR $HOME/

ENTRYPOINT ["tini", "-g", "start.sh", "--"]
