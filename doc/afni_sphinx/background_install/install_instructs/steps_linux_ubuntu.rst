.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_ubuntu:


**Ubuntu Linux (15.10 and earlier)**: *The essential system setup*
==================================================================

.. contents:: :local:

Here we describe installation and system setup for reasonably modern
Ubuntu Linux versions, up through version 15.10 (Wily Werewolf).  For
later systems, please see the :ref:`instructions here
<install_steps_linux_ubuntu16>`.

Several of the following steps are version-dependent, so we list
parallel instructions for each.

Install prerequisite packages
-----------------------------

* *For versions 15.04 and earlier*::
   
    sudo apt-get install -y tcsh libxp6 xfonts-base python-qt4             \
                            libmotif4 libmotif-dev motif-clients           \
                            gsl-bin netpbm xvfb gnome-tweak-tool           \
                            libjpeg62 xterm
    sudo apt-get update

* *For version 15.10*::
   
    sudo apt-get install -y tcsh xfonts-base python-qt4                    \
                            libmotif4 libmotif-dev motif-clients           \
                            gsl-bin netpbm xvfb gnome-tweak-tool libjpeg62
    sudo apt-get update
    sudo ln -s /usr/lib/x86_64-linux-gnu/libgsl.so /usr/lib/libgsl.so.0
    sudo dpkg -i http://mirrors.kernel.org/ubuntu/pool/main/libx/libxp/libxp6_1.0.2-2_amd64.deb
    sudo apt-get install -f

.. _setup_Ubu_tcsh:
Make "tcsh" default shell (optional/recommended)
------------------------------------------------

::

   chsh -s /usr/bin/tcsh

Install AFNI binaries
---------------------

::

  cd
  curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
  tcsh @update.afni.binaries -package linux_openmp_64 -do_extras

These commands: download and unpack the current binaries into your
``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the binary package has already been downloaded, one
          can use ``-local_package``, followed by the location+name
          of the binary file, e.g.::

            tcsh @update.afni.binaries -local_package linux_openmp_64.tgz -do_extras

Reboot
------

Consider a 'reboot' at this point.  That would deal with
system updates, the change in login shell, and an updated path::

   reboot

Install R
---------

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

Make AFNI/SUMA profiles
-----------------------

.. include:: substep_profiles.rst

Prepare for Bootcamp (semi-optional)
------------------------------------

.. include:: substep_bootcamp.rst

Evaluate setup/system (important!)
----------------------------------

.. include:: substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst


