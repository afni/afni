
# continue the AFNI setup : user level work
# -- download (from github) and execute the tcsh setup script

# this syntax is left generic for zsh or tcsh (|& will not work in bash)


curl -O https://raw.githubusercontent.com/afni/afni/master/src/other_builds/OS_notes.macos_12_b_user.tcsh

# if no previous brew setup, do so
if ( ! $?HOMEBREW_PREFIX ) then
   # set for any login shell (users might need a logout/login to apply)
   (echo; echo 'eval "$(/usr/local/bin/brew shellenv)"')      >> $HOME/.zprofile
   (echo; echo 'eval "$(/usr/local/bin/brew shellenv bash)"') >> $HOME/.bashrc
   (echo; echo 'eval `/usr/local/bin/brew shellenv tcsh`')    >> $HOME/.login

   # and in our current shell env
   eval "$(/usr/local/bin/brew shellenv)"
endif

\rm -f out.mac_12_b_user.txt
tcsh -xef OS_notes.macos_12_b_user.tcsh |& tee out.mac_12_b_user.txt

