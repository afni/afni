#!/bin/tcsh -e

set label = ""

if ( "${label}" == "" ) then
    echo ""
    echo "** ERROR: need label of branch name to build"
    echo ""
    exit 1
endif

# use ubuntu 20.04 build now!
set thedate = `date +%Y_%m_%d_%H_%m_%s`

# make sure git repo is uptodate
cd ~/AFNI/afni_${label}/src
git fetch
git checkout ${label}
git pull origin ${label}

cd

# new tempdir for build
mkdir BUILDNEW_${label}

echo "++ Sync git repo to build src (rsync)"
rsync -av --exclude=".*" ~/AFNI/afni_${label}/ ~/BUILDNEW_${label}

touch BUILDNEW_${label}

cd BUILDNEW_${label}/src
#cp other_builds/Makefile.linux_ubuntu_12_64_OMP Makefile
cp other_builds/Makefile.linux_ubuntu_16_64_glw_local_shared Makefile
#cp Makefile.linux_ubuntu_16_64 Makefile

make vastness |& tee ~/o.build_src_${label}_${thedate}.txt

if ($status) then
    echo "** ERROR IN '${label}' BUILD"
    exit 1
endif

echo "++ Done with '${label}' build--- continuing"

cd 

# push and replace
if ( -e afni_build_${label} ) then
    mv afni_build_${label} afni_build_${label}_${thedate}
endif

mv ~/BUILDNEW_${label} ~/afni_build_${label}
touch ~/afni_build_${label}

cat <<EOF
++ Done with '${label}' build:

   cd ~/afni_build_${label}/src

EOF


#apsearch -update_all_afni_help
