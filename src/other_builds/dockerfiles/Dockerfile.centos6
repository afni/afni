FROM centos:6

ENV AFNI_VERSION master
ENV INSTALLDIR /usr/local/afni
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
        openmotif-devel \
        expat-devel \
        compat-gcc-34 \
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

RUN ln -sf /usr/bin/x86_64-redhat-linux-gcc34 /usr/bin/x86_64-redhat-linux-gcc-34

RUN set -x \
    && git clone https://github.com/afni/afni.git \
    && cd afni \
    && git checkout "$AFNI_VERSION" \
    && cd src \
    && cp Makefile.linux_openmp_64 Makefile \
    && perl -p -i -e 's/^USE_LOCAL_X_TREE/#USE_LOCAL_X_TREE/' Makefile \
    && make INSTALLDIR="$INSTALLDIR" vastness

RUN useradd afni
WORKDIR /home/afni
USER afni

CMD ["/bin/bash"]
