# Quick build setup script 3/3.  Should match with 'steps_linux_rocky_8.rst'
# From bash shell:
#   tcsh OS_notes.linux_rocky_8_c_nice.tcsh 2>&1 | tee o.rocky_8_c.txt
# From tcsh shell:
#   tcsh OS_notes.linux_rocky_8_c_nice.tcsh |& tee o.rocky_8_c.txt


echo "++ Set up tab autocompletion for tcsh"

echo ""                            >> ~/.cshrc
echo 'set filec'                   >> ~/.cshrc
echo 'set autolist'                >> ~/.cshrc
echo 'set nobeep'                  >> ~/.cshrc


echo "++ Make t/csh aliases for colorful listing of files/dirs: ls, ll, ltr"

echo 'alias ls ls --color=auto'    >> ~/.cshrc
echo 'alias ll ls --color -l'      >> ~/.cshrc
echo 'alias ltr ls --color -ltr'   >> ~/.cshrc


echo "++ Make bash aliases for colorful listing of files/dirs: ls, ll, ltr"

echo ""                            >> ~/.bashrc
echo 'alias ls="ls --color"'       >> ~/.bashrc
echo 'alias ll="ls --color -l"'    >> ~/.bashrc
echo 'alias ltr="ls --color -ltr"' >> ~/.bashrc

echo "++ Done with niceifying terminal part"
