#!/bin/tcsh

# Quick build setup script 2/3.
# From bash shell:
#   sudo bash OS_notes.linux_fedora_b_user.txt 2>&1 | tee o.fedora_b.txt
# From tcsh shell:
#   sudo bash OS_notes.linux_fedora_b_user.txt |& tee o.fedora_b.txt

echo "++ Get AFNI binaries"

cd
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -package linux_openmp_64 -do_extras

source ~/.cshrc


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

# in case R's brms didn't install first time
Rscript -e "install.packages(c('Rcpp','brms'), dependencies = TRUE, INSTALL_opts = '--no-lock')"

set asc  = ~/o.afni_system_check.txt
echo "++ Run system check, saving to: ${asc}"

afni_system_check.py -check_all > ${asc}

echo "++ Done with 2nd part of install"
