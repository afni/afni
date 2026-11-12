#!/bin/bash

##################################################
## 11/05/2026 Justin Rajendra
## build afni_docker and push to docker hub
## x86 and ARM universal build
## must be run from the directory with Dockerfile
## use "docker login..." or saved credentials

afni_ver=$(curl -s "https://raw.githubusercontent.com/afni/afni/refs/heads/master/src/AFNI_version_base.txt")
disco_docker_ver=$(curl -s "https://raw.githubusercontent.com/afni/afni/refs/heads/master/src/docker/disco_docker_version.txt")

docker build --platform=linux/amd64,linux/arm64 \
       -t afni/afni_docker_universal:${disco_docker_ver} \
       -t afni/afni_docker_universal:latest \
       -t afni/afni_docker_universal:${afni_ver} \
       --progress=plain . 2>&1 | tee docker.build.log.txt

docker image push --all-tags afni/afni_docker_universal \
       2>&1 | tee docker.push.log.txt

