#!/usr/bin/tcsh
#
# Perform a user-level initialization for AFNI.
# This is now a tcsh script.
#
# assumes: R, python, tcsh, CommandLineTools
#
#   - check OS type, CPU type, R version
#   - set up terminal preferences
#   - install initial anyos_text_atlas AFNI package
#   - init dot files
#   - build AFNI
#   - install R packages
#   - minor AFNI bits
#   - run afni_system_check.py -check_all, saving the text output

# ---------------------------------------------------------------------------
# note system info

set os = `uname -s`
if ( $status ) then
   echo "** failed to set OS type: uname -s"
   exit 1
endif
if ( "$os" != Darwin ) then
   echo "** this is meant for a mac, bad 'uname -s'"
   exit 1
endif

set cpu  = `uname -m`
if ( $status ) then
   echo "** failed to set CPU: uname -m"
   exit 1
endif

set rver = `R --version | head -n 1 | cut -d ' ' -f 3`
if ( $status ) then
   echo "** failed to set R version: R --version"
   exit 1
endif


echo "OS    : $os"
echo "CPU   : $cpu"
echo "R ver : $rver"
echo ""

# ---------------------------------------------------------------------------
# set up terminal

defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
defaults write org.x.X11 wm_ffm -bool true
defaults write com.apple.Terminal FocusFollowsMouse -string YES

# ----------------------------------------------------------------------
# install initial AFNI package (no binaries), to be able to run AFNI scripts
# install under initial abin (relevant if we -do_extras)

curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -no_recur -package anyos_text_atlas \
        -bindir $HOME/abin

~/abin/init_user_dotfiles.py -shell_list bash zsh tcsh \
        -do_updates path apsearch -dir_bin ~/abin

# put AFNI in PATH
source ~/.cshrc


# ----------------------------------------------------------------------
# build AFNI, using top level directory $HOME/afni_build

if ( "$cpu" == "x86_64" ) then
   set package = macos_12_x86_64
else
   set package = macos_13_ARM_clang
endif

build_afni.py -build_root $HOME/afni_build -package $package

# and make sure we can see the new programs
rehash

# ----------------------------------------------------------------------
# install R packages
# (here it is under home directory, but might want at system or conda)
# (going this route, R_LIBS must be set in shell)

setenv R_LIBS $HOME/sw/R-$rver

echo "export R_LIBS=$R_LIBS" >> ~/.zshrc
echo "export R_LIBS=$R_LIBS" >> ~/.bashrc
echo "setenv R_LIBS $HOME/sw/R-$rver" >> ~/.cshrc
mkdir -p $R_LIBS

rPkgsInstall -pkgs ALL |& tee out.rPkgsInstall.txt

# ----------------------------------------------------------------------
# verify that Xvfb is in the PATH
which Xvfb >& /dev/null
if ( $status ) then
   # add /opt/X11/bin to PATH
   echo 'export PATH=$PATH:/opt/X11/bin' >> ~/.zshrc
   echo 'export PATH=$PATH:/opt/X11/bin' >> ~/.bashrc
   echo 'setenv PATH ${PATH}:/opt/X11/bin' >> ~/.cshrc
endif

# ----------------------------------------------------------------------
# misc, for ASC whining

cp ~/abin/AFNI.afnirc ~/.afnirc
suma -update_env
apsearch -update_all_afni_help

# ----------------------------------------------------------------------
# check the state of things

afni_system_check.py -check_all |& tee out.ASC.txt

# ----------------------------------------------------------------------
# suggest what users might do to use AFNI right now
echo ""
echo "to use AFNI now (in zsh), start with:"
echo '   source ~/.zshrc'
echo ""

