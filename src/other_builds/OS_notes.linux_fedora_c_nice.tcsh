#!/bin/tcsh

# Quick build setup script 3/3.
# From bash shell:
#   tcsh OS_notes.linux_fedora_c_nice.tcsh 2>&1 | tee o.ubu_20_c.txt
# From tcsh shell:
#   tcsh OS_notes.linux_fedora_c_nice.tcsh |& tee o.ubu_20_c.txt



echo ""             >> ~/.cshrc
echo 'set filec'    >> ~/.cshrc
echo 'set autolist' >> ~/.cshrc
echo 'set nobeep'   >> ~/.cshrc

echo 'alias ls ls --color=auto' >> ~/.cshrc
echo 'alias ll ls --color -l'   >> ~/.cshrc
echo 'alias ltr ls --color -ltr'   >> ~/.cshrc

echo ""                         >> ~/.bashrc
echo 'alias ls="ls --color"'    >> ~/.bashrc
echo 'alias ll="ls --color -l"' >> ~/.bashrc
echo 'alias ltr="ls --color -ltr"' >> ~/.bashrc