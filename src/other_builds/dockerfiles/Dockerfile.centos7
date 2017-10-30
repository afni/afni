FROM centos:7

ENV AFNI_VERSION master
ENV INSTALLDIR /opt/afni
ENV AFNI_PLUGINPATH "$INSTALLDIR"
ENV PATH "$INSTALLDIR:${PATH}"

RUN yum makecache \
    && yum -y update \
    && yum -y install git \
        gcc \
        make \
        m4 \
        zlib-devel \
        libXt-devel \
        libXext-devel \
        libXmu-devel \
        motif-devel \
        motif-static \
        expat-devel \
        expat-static \
        tcsh \
        libXpm-devel \
        gsl-devel \
        mesa-libGL-devel \
        mesa-libGLU-devel \
        libXi-devel \
        glib2-devel \
        gcc-c++ \
        netpbm-devel \
        gcc-gfortran \
    && yum clean all

RUN set -x \
    && git clone https://github.com/afni/afni.git \
    && cd afni \
    && git checkout "$AFNI_VERSION" \
    && cd src \
    && cp Makefile.linux_openmp_64 Makefile \
    && perl -p -i -e 's/^USE_LOCAL_X_TREE/#USE_LOCAL_X_TREE/' Makefile \
    && perl -p -i -e 's/^CCOLD.*/CCOLD  = \$\(CC\)/' Makefile \
    && perl -p -i -e 's/^LLIBS  = -lmri -lf2c \$\(XLIBS\)/LLIBS  = -lmri -lf2c \$\(XLIBS\) -lfontconfig/' Makefile \
    && make INSTALLDIR="$INSTALLDIR" vastness

RUN useradd afni
WORKDIR /home/afni
USER afni

CMD ["/bin/bash"]
