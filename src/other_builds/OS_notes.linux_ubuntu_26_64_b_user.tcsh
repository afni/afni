# Quick build setup script 2/3. Should match with 'steps_linux_ubuntu26.rst'
# From bash shell:
#   tcsh OS_notes.linux_ubuntu_26_64_b_user.tcsh 2>&1 | tee o.ubu_26_b.txt
# From tcsh shell:
#   tcsh OS_notes.linux_ubuntu_26_64_b_user.tcsh |& tee o.ubu_26_b.txt

# ------------------------------------------------------------------------

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

set rver = `R --version | head -n 1 | cut -d ' ' -f 3`
if ( $status ) then
   echo "** failed to set R version: R --version"
   exit 1
endif

echo "R ver : $rver"
echo ""

if ( ! $?R_LIBS ) then
   echo "++ setting R_LIBS=$HOME/sw/R-$rver"
   # start by setting R_LIBS in shell
   setenv R_LIBS $HOME/sw/R-$rver

   echo "export R_LIBS=$R_LIBS" >> ~/.bashrc
   echo "setenv R_LIBS $HOME/sw/R-$rver" >> ~/.cshrc
else
   echo "-- already have R_LIBS=$R_LIBS"
endif

echo "++ building R libraries: rPkgsInstall -pkgs ALL"
\mkdir -p $R_LIBS
rPkgsInstall -pkgs ALL |& tee out.rPkgsInstall.txt

# ------------------------------------------------------------------------

set asc  = ~/o.afni_system_check.txt
echo "++ Run system check, saving to: ${asc}"
afni_system_check.py -check_all > ${asc}

echo "++ Done with 2nd part of install"
