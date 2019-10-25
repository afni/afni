#!/bin/tcsh



# make sure git repo is uptodate
cd ~/AFNI/afni/src
git pull origin master

cd

# new tempdir for build
mkdir BUILDNEW

echo "++ Sync git repo to build src (rsync)"
rsync -av --exclude=".*" ~/AFNI/afni/ ~/BUILDNEW

cd BUILDNEW/src
cp other_builds/Makefile.linux_ubuntu_12_64_OMP Makefile
make vastness

echo "++ Done with build!"

cd 

# push and replace
if ( -e afni_build ) then
    set thedate = `date +%Y_%m_%d`
    mv afni_build afni_build_$thedate
endif

mv ~/BUILDNEW ~/afni_build

apsearch -update_all_afni_help
