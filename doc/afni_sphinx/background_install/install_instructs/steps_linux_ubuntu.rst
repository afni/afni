.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_ubuntu:


**Ubuntu Linux (15.10 and earlier)**: *The essential system setup*
==================================================================

.. contents:: :local:

What to do?
-----------

Here we describe a complete AFNI installation and system setup for
reasonably modern Ubuntu Linux versions, up through version 15.10
(Wily Werewolf).  For later systems, please see the :ref:`instructions
here <install_steps_linux_ubuntu16>`.

Each step involves either:
 A. copying+pasting commands (from the green fields) into the
    terminal, or
 #. clicking on a link to download a package.

Some steps depend on the OS version. 

We assume the user has admin privileges (can run ``sudo ...``), and
some steps require an internet connection.

If you want to know what shell you are using (e.g., ``bash`` or
``tcsh``), type::

  echo $0

**IMPORTANT:** Please:
 i. Do all steps.
 #. Run ``afni_system_check.py -check_all`` in the "Evaluate" stage.
 #. *And* follow the system check recommendations in the output
    "Please Fix" section.

Install prerequisite packages
-----------------------------

* *For versions 15.04 and earlier*, copy+paste::
   
    sudo apt-get install -y tcsh libxp6 xfonts-base python-qt4             \
                            libmotif4 libmotif-dev motif-clients           \
                            gsl-bin netpbm xvfb gnome-tweak-tool           \
                            libjpeg62 xterm
    sudo apt-get update

* *For version 15.10*, copy+paste::
   
    sudo apt-get install -y tcsh xfonts-base python-qt4                    \
                            libmotif4 libmotif-dev motif-clients           \
                            gsl-bin netpbm xvfb gnome-tweak-tool libjpeg62
    sudo apt-get update
    sudo ln -s /usr/lib/x86_64-linux-gnu/libgsl.so /usr/lib/libgsl.so.0
    sudo dpkg -i http://mirrors.kernel.org/ubuntu/pool/main/libx/libxp/libxp6_1.0.2-2_amd64.deb
    sudo apt-get install -f

**Purpose:** Installs a lot of packages that AFNI depends on (so we
don't have to reinvent the wheel!).

.. _setup_Ubu_tcsh:
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
  tcsh @update.afni.binaries -package linux_openmp_64 -do_extras

**Purpose:** These commands: download and unpack the current binaries
into your ``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the binary package has already been downloaded, one
          can use ``-local_package``, followed by the location+name
          of the binary file, e.g.::

            tcsh @update.afni.binaries -local_package linux_openmp_64.tgz -do_extras

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
    environment variable ``$R_LIBS``, which refers to a directory that
    will contain the R packages.  That variable should always be set,
    both to specify where to install the packages and where to read
    them from later (when running R programs).  The file obtained
    using ``curl`` contains instructions to add a more uptodate set of
    R libraries to the source list.

#. Copy+paste the following::
     
     rPkgsInstall -pkgs ALL

   **Purpose:** Get specific R packages needed for AFNI programs.
   
.. ---------- HERE/BELOW: copy for all installs --------------

Make AFNI/SUMA profiles
-----------------------

.. include:: substep_profiles.rst

Prepare for Bootcamp
------------------------------------

.. include:: substep_bootcamp.rst

Evaluate setup/system (**important!**)
----------------------------------

.. include:: substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst


