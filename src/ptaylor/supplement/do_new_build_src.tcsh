#!/bin/tcsh

# simple way to update local binaries on ubuntu machine

# make sure git repo is uptodate
cd ~/AFNI/afni/src
git pull origin master

cd

# new tempdir for build
mkdir BUILDNEW

echo "++ Sync git repo to build doc (rsync)"
rsync -av --exclude=".*" ~/AFNI/afni/src/ ~/BUILDNEW

cd BUILDNEW
cp other_builds/Makefile.linux_ubuntu_12_64_OMP Makefile
make cleanest
make vastness

cd 

# push and replace
if ( -e afni_src ) then
    set thedate = `date +%Y_%m_%d`
    mv afni_src afni_src_$thedate
endif

mv ~/BUILDNEW ~/afni_src
apsearch -update_all_afni_help
