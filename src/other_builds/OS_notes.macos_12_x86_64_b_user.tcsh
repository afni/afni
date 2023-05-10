#!/bin/tcsh

# *** WARNING: using [t]csh syntax


# ----------------------------------------------------------------------
# intstall initial AFNI package, to be able to run AFNI scripts
# install under initial abin (relevant if we -do_extras)
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -no_recur -package anyos_text_atlas \
        -bindir $HOME/abin -do_dotfiles

# ----------------------------------------------------------------------
# be sure python and AFNI are in current PATH
setenv PATH /usr/local/opt/python/libexec/bin:$HOME/abin:${PATH}
rehash

# ----------------------------------------------------------------------
# alternate to -do_dotfiles:
# - init home dot files with no DYLD_LIBRARY_PATH, since doing local build
#
# init_user_dotfiles.py -shell_list zsh tcsh -do_updates path apsearch


# ----------------------------------------------------------------------
# install R packages
# (here it is under home directory, but might want at system or conda)
# (going this route, R_LIBS must be set in shell)
setenv R_LIBS ~/R-4.2.3
mkdir $R_LIBS
rPkgsInstall -pkgs ALL |& tee out.rPkgsInstall.txt

# consider : @afni_R_package_install -afni -shiny -bayes_view


# ----------------------------------------------------------------------
# build AFNI, using top level directory $HOME/afni_build
build_afni.py -build_root $HOME/afni_build -package macos_12_x86_64


# ... to be continued... (e.g. rsync from build, prep for and run ASC.py)
#

