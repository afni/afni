# Quick build setup script 2/3. Should match with 'steps_linux_ubuntu24.rst'
# From bash shell:
#   tcsh OS_notes.linux_ubuntu_24_64_b_user.tcsh 2>&1 | tee o.ubu_24_b.txt
# From tcsh shell:
#   tcsh OS_notes.linux_ubuntu_24_64_b_user.tcsh |& tee o.ubu_24_b.txt

echo "++ Get AFNI binaries"

cd
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -package linux_ubuntu_24_64 -do_extras

# put the new binaries into the PATH
if ( -f ~/.tcshrc ) then
   source ~/.tcshrc
else if ( -f ~/.cshrc ) then
   source ~/.cshrc
else
   echo "-- no .cshrc file, assuming afni is already in PATH"
endif
rehash

# ------------------------------------------------------------------------

echo "++ Prepare to install R and its packages (will take a while)"

setenv R_LIBS $HOME/R
mkdir  $R_LIBS
echo  'export R_LIBS=$HOME/R' >> ~/.bashrc
echo  'setenv R_LIBS ~/R'     >> ~/.cshrc

rPkgsInstall -pkgs ALL

# ------------------------------------------------------------------------

set asc  = ~/o.afni_system_check.txt
echo "++ Run system check, saving to: ${asc}"
afni_system_check.py -check_all > ${asc}

echo "++ Done with 2nd part of install"
