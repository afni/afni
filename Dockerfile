FROM ubuntu:bionic-20180526@sha256:c8c275751219dadad8fa56b3ac41ca6cb22219ff117ca98fe82b42f24e1ba64e
USER root
RUN apt-get update -y ; apt-get upgrade -y

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get install -y tcsh vim
RUN apt-get install -y libxmu-dev libxt-dev libmotif-dev
RUN apt-get install -y libxpm-dev libxi-dev libxmhtml-dev
RUN apt-get install -y libglw1-mesa-dev libglu1-mesa-dev
RUN apt-get install -y libglib2.0-dev libgsl-dev
RUN apt-get install -y r-base m4
RUN apt-get install -y python-qt4 git
RUN apt-get install -yq --no-install-recommends gcc
RUN apt-get install -yq --no-install-recommends g++

#######################################
# this copies source code from the host
# into the image and invalidates the cache
RUN mkdir /usr/afni_build_dir
WORKDIR /usr/afni_build_dir
RUN mkdir src .git
ADD src src
ADD .git .git

ENV PATH=$PATH:/usr/afni_build_dir/src/afni_src/abin
ARG VERSION
# RUN git checkout "${VERSION}"
WORKDIR /usr/afni_build_dir/src

# o files are ignored by the repo but will
# cause the build to fail.
RUN make -f  Makefile.linux_ubuntu_16_64  afni_src.tgz \
    && tar zxvf afni_src.tgz \
    && cd afni_src \
    && cp Makefile.linux_ubuntu_16_64 Makefile
WORKDIR /usr/afni_build_dir/src/afni_src
# RUN make libmri.so
RUN make itall && mv linux_ubuntu_16_64 /opt/abin
WORKDIR /opt/abin