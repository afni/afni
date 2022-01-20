# Quick build setup script 2/3. Should match with 'steps_linux_ubuntu20.rst'
# From bash shell:
#   tcsh OS_notes.linux_ubuntu_20_64_b_user.tcsh 2>&1 | tee o.ubu_20_b.txt
# From tcsh shell:
#   tcsh OS_notes.linux_ubuntu_20_64_b_user.tcsh |& tee o.ubu_20_b.txt

echo "++ Get AFNI binaries"

cd
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -package linux_ubuntu_16_64 -do_extras

source ~/.cshrc


echo "++ Download Bootcamp data, **if** it doesn't appear to exist already"

if ( 1 && ! -d ~/AFNI_data6 && ! -d ~/AFNI_demos ) then
   echo "++ No ~/AFNI_data6 and ~/AFNI_demo dirs already,"
   echo "   so will download+install the Bootcamp data CD.tgz"

   curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
   tar xvzf CD.tgz
   cd CD
   tcsh s2.cp.files . ~
   cd ..
else
   echo "+* WARN: Finding ~/AFNI_data6 and ~/AFNI_demo dirs already,"
   echo "         so I will *not* download+install the Bootcamp data CD.tgz"
endif

echo "++ Prepare to install R and its packages (will take a while)"

setenv R_LIBS $HOME/R
mkdir  $R_LIBS
echo  'export R_LIBS=$HOME/R' >> ~/.bashrc
echo  'setenv R_LIBS ~/R'     >> ~/.cshrc

rPkgsInstall -pkgs ALL

# in case R's brms didn't install first time
Rscript -e "install.packages(c('Rcpp','brms'), dependencies = TRUE, INSTALL_opts = '--no-lock')"

echo "++ Run system check, saving to: ${asc}"
set asc  = ~/o.afni_system_check.txt
afni_system_check.py -check_all > ${asc}

echo "++ Done with 2nd part of install"
