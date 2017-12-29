
To improve your life when using the terminal, copy+paste these::

  echo 'set filec'    >> ~/.cshrc
  echo 'set autolist' >> ~/.cshrc
  echo 'set nobeep'   >> ~/.cshrc
  
  echo 'alias ls ls --color=auto' >> ~/.cshrc
  echo 'alias ll ls --color -l'   >> ~/.cshrc
  echo 'alias ls="ls --color"'    >> ~/.bashrc
  echo 'alias ll="ls --color -l"' >> ~/.bashrc

**Purpose:** The first commands set up ``tab`` autocompletion for
``tcsh`` (which should already be enabled for ``bash`` users by
default). The second set of commands make aliases so that different
types of files ("normal" files, zipped files, executables, et al.)
and directories are shown using different colors and boldness.  It
makes it *much* easier to navigate on a terminal, IMHO.
