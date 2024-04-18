#!/bin/tcsh -e

set label  = $1

# check that label was input
if ( "${label}" == "" ) then
cat <<EOF
** ERROR: need to have 1 command line arg (the branch name)
EOF
    exit 1
endif


set agbase = ${HOME}/AFNI/afni

# use ubuntu 20.04 build now!
set thedate = `date +%Y_%m_%d_%H_%m_%s`

# make sure git repo is uptodate
cd ${agbase}/src
git fetch
git checkout ${label}
if ( $status ) then
    echo "** Failure to find branch: ${label}"
    exit 1
endif
git pull origin ${label}

if ($status) then
    echo "** ERROR IN '${label}' BUILD: git start failed"
    exit 1
endif

cd

# new tempdir for build
mkdir BUILDNEW_${label}

if ($status) then
    echo "** ERROR IN '${label}' BUILD: build failed"
    exit 1
endif

echo "++ Sync git repo to build src (rsync)"
rsync -av --exclude=".*" ${agbase}/ ~/BUILDNEW_${label}

touch BUILDNEW_${label}

cd BUILDNEW_${label}/src
#cp other_builds/Makefile.linux_ubuntu_12_64_OMP Makefile
cp other_builds/Makefile.linux_ubuntu_16_64_glw_local_shared Makefile
#cp Makefile.linux_ubuntu_16_64 Makefile

if ($status) then
    echo "** ERROR IN '${label}' BUILD: copy failed"
    exit 2
endif

make vastness |& tee ~/o.build_src_${label}_${thedate}.txt

if ($status) then
    echo "** ERROR IN '${label}' BUILD: make failed"
    exit 3
endif

echo "++ Done with '${label}' build--- continuing"

cd 

# push and replace
if ( -e afni_build_${label} ) then
    mv afni_build_${label} afni_build_${label}_${thedate}
endif

mv ~/BUILDNEW_${label} ~/afni_build_${label}
touch ~/afni_build_${label}

# get NiiVue
cd ~/afni_build_${label}/src/linux_ubuntu_16_64_glw_local_shared
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/niivue_afni.umd.js
cd -

cat <<EOF
++ Done with '${label}' build:

     cd ~/afni_build_${label}/src

   maybe move new build in place (backing up old, and run help update): 

     mv ~/afni_build ~/afni_build_GOOD_${thedate}
     mv ~/afni_build_${label} ~/afni_build

     apsearch -update_all_afni_help

EOF

exit 0
#apsearch -update_all_afni_help
