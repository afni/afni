FROM ubuntu:16.04

ENV AFNI_VERSION master
ENV INSTALLDIR /opt/afni
ENV AFNI_PLUGINPATH "$INSTALLDIR"
ENV PATH "$INSTALLDIR:${PATH}"

RUN apt-get update \
    && apt-get -y upgrade \
    && apt-get -y install git \
        curl \
        gcc \
        g++ \
        make \
        m4 \
        zlib1g-dev \
        libxt-dev \
        libxext-dev \
        libxmu-headers \
        libmotif-dev \
        libxpm-dev \
        tcsh \
        libgsl-dev \
        mesa-common-dev \
        libglu1-mesa-dev \
        libxi-dev \
        libnetpbm10-dev \
        libglib2.0-dev \
    && rm -rf /var/lib/apt/lists/*

RUN set -x \
    && git clone https://github.com/afni/afni.git \
    && cd afni \
    && git checkout "$AFNI_VERSION" \
    && cd src \
    && cp Makefile.linux_openmp_64 Makefile \
    && perl -p -i -e 's/^USE_LOCAL_X_TREE/#USE_LOCAL_X_TREE/' Makefile \
    && perl -p -i -e 's/^CCOLD.*/CCOLD  = \$\(CC\)/' Makefile \
    && perl -p -i -e 's/^LGIFTI.*/LGIFTI    = \/usr\/lib\/x86_64-linux-gnu\/libexpat.a/' Makefile \
    && perl -p -i -e 's/XLIBS = \$\(XROOT\)\/lib64\/libXm.a -lXt/XLIBS = \$\(XROOT\)\/lib\/x86_64-linux-gnu\/libXm.a -lXt/' Makefile \
    && perl -p -i -e 's/^LFLAGS.*/LFLAGS = -L\. -L\/usr\/lib -L\/usr\/lib\/x86_64-linux-gnu/' Makefile\
    && perl -p -i -e 's/^PLFLAGS.*/PLFLAGS       = -rdynamic -L\. -L\/usr\/lib -L\/usr\/lib\/x86_64-linux-gnu/' Makefile \
    && perl -p -i -e 's/^LLIBS.*/LLIBS  = -lmri -lf2c \$\(XLIBS\) -lXft \/usr\/lib\/x86_64-linux-gnu\/libXp.so.6 -lXpm -lfontconfig -lXext \/usr\/lib\/x86_64-linux-gnu\/libXmu.so.6 -lSM -lICE -lX11 \\/' Makefile \
    && perl -p -i -e 's/(^SUMA_INCLUDE_PATH.*)/$1 -I\/usr\/lib\/x86_64-linux-gnu\/glib-2.0\/include/' Makefile \
    && curl --retry 5 -o /tmp/libxp6.deb -sSL  https://mirrors.kernel.org/debian/pool/main/libx/libxp/libxp6_1.0.2-2_amd64.deb \
    && dpkg -i /tmp/libxp6.deb \
    && rm -f /tmp/libxp6.deb \
    && make INSTALLDIR="$INSTALLDIR" vastness

RUN useradd afni
WORKDIR /home/afni
USER afni

CMD ["/bin/bash"]
