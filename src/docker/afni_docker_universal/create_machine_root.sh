#!/bin/bash

## 9/2026 Justin Rajendra
## admin level install for afni docker on x86 AND arm
## runs from the Dockerfile as root

## echo commands to terminal
set -x

######################################
## update package manager and get some required packages
apt-get update
apt install -y software-properties-common
add-apt-repository universe -y
apt-get update
apt-get install -y curl tcsh sudo ffmpeg iputils-ping wget mesa-utils

######################################
## get the admin script for the afni install
curl -O https://raw.githubusercontent.com/afni/afni/master/src/other_builds/OS_notes.linux_ubuntu_26_64_a_admin.txt
bash OS_notes.linux_ubuntu_26_64_a_admin.txt

######################################
## make folders and links for mounting local host drives and local syncs
mkdir -p /usr/local/abin
chown -R afni_user:afni_user /usr/local/abin
mkdir -p /home/external/
mkdir -p /home/afni_user/
chown -R afni_user:afni_user /home/afni_user/
chown -R afni_user:afni_user /home/external/

## use the .afnirc from the host machine user's home directory if it exists
## ( -h for symbolic link )
ln -s /home/external/.afnirc /home/afni_user/.afnirc
chown -h afni_user:afni_user /home/afni_user/.afnirc
