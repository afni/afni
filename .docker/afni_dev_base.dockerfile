FROM neurodebian:nd18.04@sha256:3b3f09ca5387f479f144a2e45fb191afa9c9f7c1bd0f03ac90941834a4e5a616
# FROM thewtex/opengl:ubuntu1804@sha256:b9de45d4f594b57136f7ec3b890567ecea1421278ee4c7be80e11888bf8d23ba

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn


RUN apt-get update && apt-get install -y wget sudo locales \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
# The gpg key import is a little flaky...
# COPY .docker/neurodebian.gpg /usr/local/etc/neurodebian.gpg
# RUN wget  -O- http://neuro.debian.net/lists/bionic.us-nh.full > /etc/apt/sources.list.d/neurodebian.sources.list && \
#     apt-key add /usr/local/etc/neurodebian.gpg && \
#     (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

# Configure environment
RUN ln -sf  /bin/bash /bin/sh # use bash by default
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
    python3-rpy2 \
    python3-wxgtk4.0 \
    python3.6-dev \
    qhull-bin \
    r-base \
    tcsh \
    xvfb \
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
    git-annex-standalone \
    libtool \
    m4 \
    ncurses-dev \
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

ENV CMAKE_VER=3.14.7
RUN wget -P /opt/cmake  https://github.com/Kitware/CMake/releases/download/v${CMAKE_VER}/cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ; cd /opt/cmake \
  ; tar xzvf cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  ;rm -fr cmake-${CMAKE_VER}-Linux-x86_64.tar.gz \
  && fix-permissions /opt
ENV PATH="/opt/cmake/cmake-${CMAKE_VER}-Linux-x86_64/bin:$PATH"

RUN mkdir $PYTHONUSERBASE
# Add some more test dependencies
RUN curl -fsSL https://bootstrap.pypa.io/get-pip.py \
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
        git+git://github.com/leej3/xvfbwrapper.git@add_support_for_xquartz_and_multi_threading \
  && fix-permissions /opt

# add pdb alias ipy for easier pdb debugging
RUN echo 'alias ipy from IPython import embed;embed()' >> ~/.pdbrc
RUN mkdir $HOME/work && fix-permissions $HOME/work
WORKDIR $HOME/

ENTRYPOINT ["tini", "-g", "start.sh", "--"]

