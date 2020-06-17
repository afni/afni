#!/bin/tcsh

# Quick build setup script 2/3.
# From bash shell:
#   tcsh OS_notes.linux_ubuntu_20_64_b_user.tcsh 2>&1 | tee o.ubu_20_b.txt
# From tcsh shell:
#   tcsh OS_notes.linux_ubuntu_20_64_b_user.tcsh |& tee o.ubu_20_b.txt

echo "++ Get AFNI binaries"

cd
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -package linux_ubuntu_16_64 -do_extras

source ~/.cshrc


echo "++ Setup AFNI env vars"

cp $HOME/abin/AFNI.afnirc $HOME/.afnirc
suma -update_env


echo "++ Download Bootcamp data"

curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
tar xvzf CD.tgz
cd CD
tcsh s2.cp.files . ~
cd ..


echo "++ Prepare to install R and its packages (will take a while)"

setenv R_LIBS $HOME/R
mkdir  $R_LIBS
echo  'export R_LIBS=$HOME/R' >> ~/.bashrc
echo  'setenv R_LIBS ~/R'     >> ~/.cshrc

rPkgsInstall -pkgs ALL


set asc  = ~/o.afni_system_check.txt
echo "++ Run system check, saving to: ${asc}"

afni_system_check.py -check_all > ${asc}

echo "++ Done with 2nd part of install"
