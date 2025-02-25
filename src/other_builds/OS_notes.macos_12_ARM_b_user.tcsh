
# continue the AFNI setup : user level work
# -- download (from github) and execute the tcsh setup script

# this syntax is left generic for zsh or tcsh (|& will not work in bash)


curl -O https://raw.githubusercontent.com/afni/afni/master/src/other_builds/OS_notes.macos_12_b_user.tcsh

tcsh -xef OS_notes.macos_12_b_user.tcsh |& tee out.mac_12_b_user.txt

