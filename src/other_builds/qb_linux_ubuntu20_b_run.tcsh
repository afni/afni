#!/bin/tcsh

###  Quick build setup script 2.
###  Run from command line (no admin):
###    tcsh qb_linux_ubuntu20_b_run.tcsh



cd
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -package linux_ubuntu_16_64 -do_extras

cp $HOME/abin/AFNI.afnirc $HOME/.afnirc
suma -update_env

setenv R_LIBS $HOME/R
mkdir  $R_LIBS
echo  'export R_LIBS=$HOME/R' >> ~/.bashrc
echo  'setenv R_LIBS ~/R'     >> ~/.cshrc

rPkgsInstall -pkgs ALL

curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
tar xvzf CD.tgz
cd CD
tcsh s2.cp.files . ~
cd ..

echo "++ Done with 2nd part of install"
