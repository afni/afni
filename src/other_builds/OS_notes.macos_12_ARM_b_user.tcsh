
# continue the AFNI setup : user level work
# -- download (from github) and execute the tcsh setup script

# this syntax is left generic for zsh or tcsh (|& will not work in bash)

printf "" > out.mac_12_b_user.txt
curl -O https://raw.githubusercontent.com/afni/afni/master/src/other_builds/OS_notes.macos_12_b_user.tcsh

if ( $status ) then
    echo "** ERROR: curl failed" |& tee -a out.mac_12_b_user.txt
    exit 1
endif

# if no previous brew setup, do so
set FOUND = `grep shellenv ~/.zshrc | grep brew | wc -l`
echo "++ num brew shellenv found : $FOUND" |& tee -a out.mac_12_b_user.txt

if ( ! $FOUND ) then
   echo "++ setup login shells with brew path" |& tee -a out.mac_12_b_user.txt

   # set for any login shell (users might need a logout/login to apply)
   (echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"')      >> $HOME/.zprofile
   (echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"')      >> $HOME/.zshrc
   (echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv bash)"') >> $HOME/.bashrc
   (echo; echo 'eval `/opt/homebrew/bin/brew shellenv tcsh`')    >> $HOME/.login

   # and in our current shell env
   eval `/opt/homebrew/bin/brew shellenv`
endif

echo "++ start building..." |& tee -a out.mac_12_b_user.txt
tcsh -xef OS_notes.macos_12_b_user.tcsh |& tee -a out.mac_12_b_user.txt

