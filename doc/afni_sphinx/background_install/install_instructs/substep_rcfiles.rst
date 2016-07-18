
The following are some suggestions for improving your terminal
experience (and greatly so, in the opinion of some).

* *For* ``tcsh`` *users*:

    The following help with using ``tab`` for autocompletion::

      echo 'set filec' >> ~/.cshrc
      echo 'set autolist' >> ~/.cshrc
      echo 'set nobeep' >> ~/.cshrc

    The following are useful aliases for having the terminal
    differentiate different types of files ("normal" files, zipped
    files, executables, et al.) and directories using colors::

      echo 'alias ls ls -G' >> ~/.cshrc
      echo 'alias ll ls -lG' >> ~/.cshrc

* *For* ``bash`` *users*:

    (Autocompletion features should be nice already.)

    The following are useful aliases for having the terminal
    differentiate different types of files ("normal" files, zipped
    files, executables, et al.) and directories using colors::

      echo 'alias ls="ls --color"' >> ~/.bashrc
      echo 'alias ll="ls --color -l"' >> ~/.bashrc
