#!/bin/tcsh

set do_push = $1

set adoc_build  = "$HOME/afni_doc"
set agit_sphinx = "$HOME/AFNI/afni_doc/"

echo "++ Make sure repo is uptodate ('git pull ...')"
cd ${agit_sphinx}
git pull origin master

echo "++ Ensure build doc exists"
cd ~

set thedate = `date +%Y_%m_%d_%H_%M_%s`

if ( -e $adoc_build ) then
    echo "++ Move existing docfile to:\n\t ${adoc_build}_$thedate"
    mv ${adoc_build} ${adoc_build}_$thedate
endif

mkdir -p $adoc_build

echo "++ Sync git repo to build doc (rsync)"
rsync -av --exclude=".*" $agit_sphinx $adoc_build

echo "++ Build the docs"
cd $adoc_build

if ( $do_push ) then
    echo "++ Build&push"
    tcsh do_doc_build_and_copy.tcsh -build -push \
    |& tee o.doc_build_push.${thedate}.txt
else
    echo "++ Build only"
    tcsh do_doc_build_and_copy.tcsh -build \
    |& tee o.doc_build_push.${thedate}.txt
endif 

# ... WILL ADD PUSH, AS WELL!

cd 


