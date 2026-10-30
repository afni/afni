#!/bin/bash

##################################################
## 10/29/2026 Justin Rajendra
## build afni_docker and push to docker hub
## x86 and ARM universal build
## must be run from the directory with Dockerfile
## must have docker connected with account to docker hub
## as of 10/2026 pushing to discoraj's docker hub

afni_ver=$(curl -s "https://raw.githubusercontent.com/afni/afni/refs/heads/master/src/AFNI_version_base.txt")
disco_docker_ver=$(curl -s "https://raw.githubusercontent.com/afni/afni/refs/heads/master/src/docker/disco_docker_version.txt")


docker build --platform=linux/amd64,linux/arm64 \
       -t discoraj/afni_docker_universal:2.0 \
       -t discoraj/afni_docker_universal:latest \
       -t discoraj/afni_docker_universal:AFNI_${afni_ver} \
       --progress=plain  . 2>&1 | tee build.log

docker image push --all-tags discoraj/afni_docker_universal