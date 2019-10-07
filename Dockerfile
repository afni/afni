# FROM ubuntu:bionic-20180526@sha256:c8c275751219dadad8fa56b3ac41ca6cb22219ff117ca98fe82b42f24e1ba64e
FROM neurodebian:bionic
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y -qq \
    && apt-get install -yq --no-install-recommends \
          ca-certificates \
          curl \
          g++ \
          gcc \
          git \
          libglib2.0-dev \
          libglu1-mesa-dev \
          libglw1-mesa-dev \
          libgsl-dev \
          libmotif-dev \
          libxi-dev \
          libxmhtml-dev \
          libxmu-dev \
          libxpm-dev \
          libxt-dev \
          m4 \
          python-dev \
          python-matplotlib \
          python-numpy \
          python-scipy \
          python-qt4 \
          python-wxgtk3.0 \
          python-rpy2 \
          python-tk \
          python-mpltoolkits.basemap \
          r-base \
          git-annex-standalone \
          tcsh \
          vim \
          rsync \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


ENV CONDA_DIR="/opt/miniconda-latest" \
    PATH="/opt/miniconda-latest/bin:$PATH"
RUN export PATH="/opt/miniconda-latest/bin:$PATH" \
    && echo "Downloading Miniconda installer ..." \
    && conda_installer="/tmp/miniconda.sh" \
    && curl -fsSL --retry 5 -o "$conda_installer" https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && bash "$conda_installer" -b -p /opt/miniconda-latest \
    && rm -f "$conda_installer" \
    && conda update -yq -nbase conda \
    && conda config --system --prepend channels conda-forge \
    && conda config --system --set auto_update_conda false \
    && conda config --system --set show_channel_urls true \
    && sync && conda clean --all && sync \
    && conda create -y -q --name neuro \
    && sync && conda clean --all && sync


ENV PATH="/opt/miniconda-latest/envs/neuro/bin:$PATH"
RUN conda install -y -q --name neuro \
           "conda-build" \
           "cmake" \
           "jpeg" \
           "libnetcdf" \
           "ninja" \
    && sync && conda clean --all && sync


# Copy AFNI source code. This can invalidate the build cache.
ARG AFNI_ROOT=/opt/afni
COPY [".", "$AFNI_ROOT/"]


RUN  mkdir -p /build
WORKDIR /build

# RUN  cmake -GNinja $AFNI_ROOT
# RUN ninja install
