
# this uses zsh syntax, and expects it to be the login shell

# ----------------------------------------------------------------------
# intstall initial AFNI package, to be able to run AFNI scripts
# install under initial abin (relevant if we -do_extras)
curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/@update.afni.binaries
tcsh @update.afni.binaries -no_recur -package anyos_text_atlas \
        -bindir $HOME/abin

~/abin/init_user_dotfiles.py -shell_list zsh tcsh \
        -do_updates path apsearch -dir_bin ~/abin

source ~/.zshrc


# ----------------------------------------------------------------------
# build AFNI, using top level directory $HOME/afni_build

build_afni.py -build_root $HOME/afni_build -package macos_12_x86_64
  # --- OR, for ARM:
build_afni.py -build_root $HOME/afni/build -package macos_13_ARM_clang

# we already have atlases, and don't need ~/afni_build/afni_atlases_dist
rsync -av $HOME/afni_build/build_src/macos_12_x86_64/ ~/abin/
  # --- OR, for ARM:
rsync -av $HOME/afni_build/build_src/macos_13_ARM_clang/ ~/abin/

# ----------------------------------------------------------------------
# misc, for ASC whining

cp ~/abin/AFNI.afnirc ~/.afnirc
suma -update_env
apsearch -update_all_afni_help

# ----------------------------------------------------------------------
# install R packages
# (here it is under home directory, but might want at system or conda)
# (going this route, R_LIBS must be set in shell)
echo 'export R_LIBS=$HOME/sw/R-4.2.3' >> ~/.zshrc
export R_LIBS=$HOME/sw/R-4.2.3
mkdir -p $R_LIBS

rPkgsInstall -pkgs ALL |& tee out.rPkgsInstall.txt

# ----------------------------------------------------------------------
# check the state of things

afni_system_check.py -check_all |& tee out.ASC.txt

