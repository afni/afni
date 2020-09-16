
FROM ubuntu:bionic

RUN apt-get update && apt-get install -y -q  \
libexpat1-dev \
zlib1g-dev \
git \
wget \
make \
gcc \
&& rm -rf /var/lib/apt/lists/*

# At least 3.11 is required
ENV CMAKE_VER=cmake-3.13.0-Linux-x86_64
RUN wget -P /cmake  https://github.com/Kitware/CMake/releases/download/v3.13.0/${CMAKE_VER}.tar.gz \
  && cd /cmake \
  && tar xzvf ${CMAKE_VER}.tar.gz \
  && rm -fr ${CMAKE_VER}.tar.gz 
ENV PATH="/cmake/${CMAKE_VER}/bin:$PATH"


RUN mkdir /gifti_clib 
RUN mkdir /gifti_build 
COPY . /gifti_clib/


WORKDIR /gifti_build
RUN cmake /gifti_clib \
    && make install \
    && ctest
