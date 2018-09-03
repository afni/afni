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
                    vim \
                    git \
                    cmake-curses-gui \
                    pkg-config && \
    curl -sSL http://neuro.debian.net/lists/xenial.us-ca.full >> /etc/apt/sources.list.d/neurodebian.sources.list && \
    apt-key add /root/.neurodebian.gpg && \
    (apt-key adv --refresh-keys --keyserver hkp://ha.pool.sks-keyservers.net 0xA5D32F012649A5A9 || true)

RUN echo deb-src http://neuro.debian.net/debian bionic main contrib> /etc/apt/sources.list.d/neurodebian.list

RUN apt-get update & apt-get -y build-dep afni


# #######################################
# # this copies source code from the host
# # into the image and invalidates the cache
ADD . /afni/
RUN  mkdir -p /build
WORKDIR /build
RUN cmake ../afni 
RUN make -j 20 install
ENV LD_LIBRARY_PATH=/usr/local/bin

