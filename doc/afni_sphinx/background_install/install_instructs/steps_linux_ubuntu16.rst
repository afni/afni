.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_ubuntu16:


**Ubuntu Linux (16.04 through 17.10)**: *The essential system setup*
=================================================================

.. contents:: :local:

Here we describe installation and system setup for modern Ubuntu Linux
versions, specifically 16.04 (Xenial Xerus).  It *may* work for later
systems, as well.  For earlier versions of Ubuntu, please see the
relevant :ref:`instructions here <install_steps_linux_ubuntu>`.

.. include:: substep_intro.rst

Install prerequisite packages
-----------------------------

a. Copy+paste::

     sudo add-apt-repository universe
     sudo apt-get update

     sudo apt-get install -y tcsh xfonts-base python-qt4       \
                             gsl-bin netpbm gnome-tweak-tool   \
                             libjpeg62 xvfb xterm vim curl     \
                             gedit evince                      \
                             libglu1-mesa-dev libglw1-mesa     \
                             libxm4 build-essential

   **Purpose:** Installs a lot of packages that AFNI depends on (so we
   don't have to reinvent the wheel!).

#. Copy+paste::

     sudo apt-get install -y gnome-terminal nautilus          \
                             gnome-icon-theme-symbolic

   **Purpose:** Useful for improving terminal behavior, especially if
   you are following the Windowsy :ref:`WSL <install_steps_windows10>`
   or (older) :ref:`BoUoW <install_steps_windows10_beta>`
   instructions. This may take a little while to complete running.

.. internal note/comment: at this moment, we are eschewing
   including 'mwm', which is the replacement for 'motif-clients'
   from earlier Ubuntu versions.  If problems arise, that might be
   useful

.. _setup_Ubu16_tcsh:
Make "tcsh" default shell (optional/recommended)
------------------------------------------------

Copy+paste::

   chsh -s /usr/bin/tcsh

**Purpose:** Makes ``tcsh`` your default shell in the terminal.

Install AFNI binaries
---------------------

Copy+paste::

   cd 
   curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
   tcsh @update.afni.binaries -package linux_ubuntu_16_64  -do_extras

**Purpose:** These commands: download and unpack the current binaries
into your ``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the binary package has already been downloaded, one
          can use ``-local_package``, followed by the location+name
          of the binary file, e.g.::

            tcsh @update.afni.binaries -local_package linux_ubuntu_16_64.tgz -do_extras

Reboot
------

Copy+paste (to reboot)::

   reboot

**Purpose:** This deals with system updates, any change in login
shell, and path updates.

Install R
---------
 
a. Copy+paste the following:

   * *for* ``tcsh``::
   
       setenv R_LIBS $HOME/R
       mkdir $R_LIBS
       echo 'setenv R_LIBS ~/R' >> ~/.cshrc
       curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
       sudo tcsh @add_rcran_ubuntu.tcsh

   * *for* ``bash``::
   
       export R_LIBS=$HOME/R
       mkdir $R_LIBS
       echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
       curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
       sudo tcsh @add_rcran_ubuntu.tcsh

   **Purpose:** Setup modern R from scratch. This relies on the
   environment variable ``$R_LIBS``, which specifies where to install
   the packages and where to read them from later (when R programs
   run).  The file obtained using ``curl`` contains instructions to
   add a more uptodate set of R libraries to the source list.

#. Copy+paste the following::
     
     rPkgsInstall -pkgs ALL

   **Purpose:** Get specific R packages needed for AFNI programs.
   
.. ---------- HERE/BELOW: copy for all installs --------------

Make AFNI/SUMA profiles
-----------------------

.. include:: substep_profiles.rst

Prepare for Bootcamp
--------------------

.. include:: substep_bootcamp.rst


Evaluate setup/system (**important!**)
-----------------------------------

.. include:: substep_evaluate.rst


Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles.rst

Also, consider running ``gnome-tweak-tool`` and changing ``Windows``
-> ``Focus Mode`` from 'click' to 'mouse'.

Also, consider extending time for screen saver: ``System Settings`` ->
``Brightness & Lock``, and set inactivity duration.

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst

