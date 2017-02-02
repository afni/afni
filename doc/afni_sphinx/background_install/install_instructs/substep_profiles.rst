
As noted in the :ref:`Technical notes <tech_notes_ENV>`, AFNI
and SUMA have a lot of default settings, controlled using
*environment variables*.  Vanilla-mode profiles with default values
are easily installed:

- For AFNI, copy the file from the main directory of binaries::

    cp $HOME/abin/AFNI.afnirc $HOME/.afnirc

- For SUMA, run the command::

    suma -update_env

  This makes and populates a profile called ``$HOME/.sumarc``.

These files can be edited to the user's heart's content, setting up
specific profile features you want when using AFNI and SUMA (e.g.,
having left=left when viewing axial slices, making default colorbars,
etc.).
