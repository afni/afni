
As noted in the :ref:`Technical notes <tech_notes_ENV>`, AFNI
and SUMA have a lot of default settings, controlled using
*environment variables*.  Vanilla-mode profiles with default values
are easily installed for both AFNI and SUMA::

    cp $HOME/abin/AFNI.afnirc $HOME/.afnirc
    suma -update_env

These files (``$HOME/.afnirc`` and ``$HOME/.sumarc``) can be edited to
the user's heart's content, setting up specific profile features you
want when using AFNI and SUMA (e.g., having left=left when viewing
axial slices, making default colorbars, etc.).
