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

# install AFNI's anyos_text_atlas package if nothing appears to be installed
if ( ! -f $HOME/abin/init_user_dotfiles.py ) then
   echo "++ installing AFNI anyos_text_atlas"
   curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
   tcsh @update.afni.binaries -no_recur -package anyos_text_atlas \
                              -bindir $HOME/abin
else
   echo "-- skipping install of AFNI anyos_text_atlas"
endif

# always init dotfiles, as it does not compound
echo "++ setting up user dotfiles"
~/abin/init_user_dotfiles.py -shell_list bash zsh tcsh \
                             -do_updates path apsearch -dir_bin ~/abin

# put AFNI in PATH, if not already there
`which init_user_dotfiles.py` >& /dev/null
if ( $status ) then
   source ~/.cshrc
endif

# ----------------------------------------------------------------------
# build AFNI, using top level directory $HOME/afni_build

if ( "$cpu" == "x86_64" ) then
   set package = macos_12_x86_64
else
   set package = macos_13_ARM
endif

# if we are in this script, always run the build
echo "++ compiling AFNI package $package"
echo "++ running: build_afni.py -build_root ~/afni_build -package $package"
build_afni.py -build_root ~/afni_build -package $package

# and make sure we can see the new programs
rehash

# ----------------------------------------------------------------------
# install R packages
# (here it is under home directory, but might want at system or conda)
# (going this route, R_LIBS must be set in shell)

if ( ! $?R_LIBS ) then
   echo "++ setting R_LIBS=$HOME/sw/R-$rver"
   setenv R_LIBS $HOME/sw/R-$rver

   echo "export R_LIBS=$R_LIBS" >> ~/.zshrc
   echo "export R_LIBS=$R_LIBS" >> ~/.bashrc
   echo "setenv R_LIBS $HOME/sw/R-$rver" >> ~/.cshrc
else
   echo "-- already have R_LIBS=$R_LIBS"
endif

if ( ! -d $R_LIBS/data.table ) then
   echo "++ building R libraries: rPkgsInstall -pkgs ALL"
   mkdir -p $R_LIBS
   rPkgsInstall -pkgs ALL |& tee out.rPkgsInstall.txt
else
   echo "-- already have directory $R_LIBS"
endif

# ----------------------------------------------------------------------
# verify that Xvfb is in the PATH
if ( -f /opt/X11/bin/Xvfb ) then
   which Xvfb >& /dev/null
   if ( $status ) then
      echo "++ adding /opt/X11/bin to PATH, for Xvfb"
      echo 'export PATH=$PATH:/opt/X11/bin' >> ~/.zshrc
      echo 'export PATH=$PATH:/opt/X11/bin' >> ~/.bashrc
      echo 'setenv PATH ${PATH}:/opt/X11/bin' >> ~/.cshrc
   else
      echo "-- already have Xvfb in PATH"
   endif
else
   echo ""
   echo "** missing /opt/X11/bin/Xvfb, this should be fixed"
   echo ""
endif

# ----------------------------------------------------------------------
# misc, for ASC whining

if ( ! -f ~/.afnirc ) then
   echo '++ running:  cp ~/abin/AFNI.afnirc ~/.afnirc'
   cp ~/abin/AFNI.afnirc ~/.afnirc
endif

if ( ! -f ~/.sumarc ) then
   echo '++ running: suma -update_env'
   suma -update_env
endif

if ( ! -f ~/.afni/help/all_progs.COMP ) then
   echo ++ running: apsearch -update_all_afni_help
   apsearch -update_all_afni_help
endif

# ----------------------------------------------------------------------
# check the state of things

echo ++ running: afni_system_check.py -check_all
afni_system_check.py -check_all |& tee out.ASC.txt

# ----------------------------------------------------------------------
# suggest what users might do to use AFNI right now
echo ""
echo "++ AFNI setup is complete, to use AFNI now (in zsh), start with:"
echo "   source ~/.zshrc"
echo ""
echo "   (or source the appropriate startup file, or just reboot to be sure)"
echo ""

