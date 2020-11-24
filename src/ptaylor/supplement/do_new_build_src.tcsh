#!/bin/tcsh

# use ubuntu 18.04 build now!
set thedate = `date +%Y_%m_%d`

# make sure git repo is uptodate
cd ~/AFNI/afni/src
git pull origin master

cd

# new tempdir for build
mkdir BUILDNEW

echo "++ Sync git repo to build src (rsync)"
rsync -av --exclude=".*" ~/AFNI/afni/ ~/BUILDNEW

touch BUILDNEW

cd BUILDNEW/src
#cp other_builds/Makefile.linux_ubuntu_12_64_OMP Makefile
cp Makefile.linux_ubuntu_16_64 Makefile

make vastness |& tee ~/o.build_src_${thedate}.txt

if ($status) then
    echo "** ERROR IN BUILD"
    exit 1
endif

echo "++ Done with build--- continuing"

cd 

# push and replace
if ( -e afni_build ) then
    mv afni_build afni_build_${thedate}
endif

mv ~/BUILDNEW ~/afni_build
touch ~/afni_build

apsearch -update_all_afni_help
