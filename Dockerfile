# FROM jupyter/base-notebook
# USER root
# ENV DEBIAN_FRONTEND=noninteractive
FROM ubuntu:xenial-20161213
# Pre-cache neurodebian key
COPY docker/files/neurodebian-archive-keyring.gpg /root/.neurodebian.gpg

# Prepare environment
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
                    curl \
                    bzip2 \
                    ca-certificates \
                    xvfb \
                    cython3 \
                    build-essential \
                    autoconf \
                    libtool \
                    pkg-config && \
    curl -sSL http://neuro.debian.net/lists/xenial.us-ca.full >> /etc/apt/sources.list.d/neurodebian.sources.list && \
    apt-key add /root/.neurodebian.gpg && \
    (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

RUN echo deb-src http://neuro.debian.net/debian bionic main contrib> /etc/apt/sources.list.d/neurodebian.list

RUN apt-get update & apt-get -y build-dep afni

RUN apt-get install cmake-curses-gui

# #######################################
# # this copies source code from the host
# # into the image and invalidates the cache
ADD . /afni/
RUN  mkdir -p /afni/build
WORKDIR /afni/build
RUN cmake   -DAFNI_BUILD_CORELIBS_ONLY=OFF -DAFNI_BUILD_LOCAL_NIFTICLIBS=ON -DBUILD_SHARED_LIBS:BOOL=ON .. && make -j 20


# WORKDIR /afni
# RUN mkdir src .git
# COPY .git .git
# RUN cd /afni/build & cmake .. && \
 # make
# # ENV PATH=$PATH:/usr/afni_build_dir/src/afni_src/abin
# # ARG VERSION
# # # RUN git checkout "${VERSION}"
# # WORKDIR /usr/afni_build_dir/src

# # # o files are ignored by the repo but will
# # # cause the build to fail.
# # RUN make -f  Makefile.linux_ubuntu_16_64  afni_src.tgz \
# #     && tar zxvf afni_src.tgz \
# #     && cd afni_src \
# #     && cp Makefile.linux_ubuntu_16_64 Makefile
# # WORKDIR /usr/afni_build_dir/src/afni_src
# # # RUN make libmri.so
# # RUN make itall && mv linux_ubuntu_16_64 abin
# # WORKDIR /usr/afni_build_dir/src/afni_src/abin
# # WORKDIR /usr/afni_build_dir/src/afni_src/abin
