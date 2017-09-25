.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_ubuntu16:


**Ubuntu Linux (16.04 and higher)**: *The essential system setup*
=================================================================

.. contents:: :local:

Here we describe installation and system setup for modern Ubuntu Linux
versions, specifically 16.04 (Xenial Xerus).  It *may* work for later
systems, as well.  For earlier versions of Ubuntu, please see the
relevant :ref:`instructions here <install_steps_linux_ubuntu>`.

**Install prerequisite packages**
---------------------------------

::

   sudo add-apt-repository universe
   sudo apt-get update

   sudo apt-get install -y tcsh xfonts-base python-qt4       \
                           gsl-bin netpbm gnome-tweak-tool   \
                           libjpeg62 xvfb xterm vim curl
   
   sudo apt-get install -y libglu1-mesa-dev libglw1-mesa     \
                           libxm4 build-essential

*Optional, but may be quite useful (esp. if you are following the*
:ref:`BoUoW <install_steps_windows10>` *instructions). This may take a
little while to complete running, but it should provide nice terminal
behavior*::

  sudo apt-get install -y gnome-terminal nautilus          \
                          gnome-icon-theme-symbolic

.. internal note/comment: at this moment, we are eschewing
   including 'mwm', which is the replacement for 'motif-clients'
   from earlier Ubuntu versions.  If problems arise, that might be
   useful

.. _setup_Ubu16_tcsh:
**Set "tcsh" as default shell (optional, but recommended)**
-----------------------------------------------------------

::

   chsh -s /usr/bin/tcsh

**Install AFNI binaries**
-------------------------

::

   cd 
   curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
   tcsh @update.afni.binaries -package linux_ubuntu_16_64  -do_extras

These commands: download and unpack the current binaries into your
``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the binary package has already been downloaded, one
          can use ``-local_package``, followed by the location+name
          of the binary file, e.g.::

            tcsh @update.afni.binaries -local_package linux_ubuntu_16_64.tgz -do_extras

**Reboot**
----------

Consider a 'reboot' at this point.  That would deal with
system updates, the change in login shell, and an updated path::

   reboot

**Get R setup**
----------------

To setup R from scratch, follow the instructions for your shell:

* *for* ``tcsh``::

    setenv R_LIBS $HOME/R
    mkdir $R_LIBS
    echo 'setenv R_LIBS ~/R' >> ~/.cshrc
    curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
    sudo tcsh @add_rcran_ubuntu.tcsh
    rPkgsInstall -pkgs ALL
   
* *for* ``bash``::
   
    export R_LIBS=$HOME/R
    mkdir $R_LIBS
    echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
    curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
    sudo tcsh @add_rcran_ubuntu.tcsh
    rPkgsInstall -pkgs ALL
   
This installs current R libraries for the group analysis programs.
This relies on the environment variable ``$R_LIBS``, which refers to a
directory that will contain the R packages.  That variable should
always be set, both to specify where to install the packages and where
to read them from later (when running R programs).  The file obtained
using ``curl`` contains instructions to add a more uptodate set of R
libraries to the source list.



.. ---------- HERE/BELOW: copy for all installs --------------

**Make AFNI/SUMA profiles**
---------------------------

.. include:: substep_profiles.rst

**Prepare for Bootcamp (semi-optional)**
----------------------------------------

.. include:: substep_bootcamp.rst


**Evaluate setup/system (important!!)**
---------------------------------------

.. include:: substep_evaluate.rst


**Niceify terminal (optional, but goood)**
------------------------------------------

.. include:: substep_rcfiles.rst

Also, consider running ``gnome-tweak-tool`` and changing ``Windows``
-> ``Focus Mode`` from 'click' to 'mouse'.

Also, consider extending time for screen saver: ``System Settings`` ->
``Brightness & Lock``, and set inactivity duration.

**Keep up-to-date (remember!)**
-------------------------------

.. include:: substep_update.rst

