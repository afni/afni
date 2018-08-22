.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_Fed_RH:


**Fedora and Red Hat Linux**: *The essential system setup*
==========================================================

.. contents:: :local:

What to do?
-----------

Here we describe installation and system setup for reasonably modern
Linux versions of Fedora (21+) and Red Hat (RHEL) 7, along with the
corresponding CentOS 7.  For most steps, internet connection is
required.

.. include:: substep_intro.rst

Install prerequisite packages
-----------------------------

* *For Fedora 21 (and higher)*, copy+paste::
   
    sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                        PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                        xorg-x11-server-Xvfb
    sudo yum update -y
   
* *For CentOS/RHEL 7*, copy+paste::

    sudo yum install -y epel-release
    sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                        PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                        libpng12 xorg-x11-server-Xvfb
    sudo yum update -y

**Purpose:** Installs a lot of packages that AFNI depends on (so we
don't have to reinvent the wheel!).
         
.. _setup_FRH_tcsh:

Make "tcsh" default shell (optional/recommended)
------------------------------------------------

Copy+paste::

   chsh -s /usr/bin/tcsh

**Purpose:** Makes ``tcsh`` your default shell in the terminal.

Install AFNI binaries
---------------------

* *For Fedora 21 (and higher)*, copy+paste::

    cd
    curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
    tcsh @update.afni.binaries -package linux_openmp_64 -do_extras

* *For CentOS/RHEL 7*, copy+paste::

    cd
    curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
    tcsh @update.afni.binaries -package linux_centos_7_64 -do_extras


**Purpose:** These commands: download and unpack the current binaries
into your ``$HOME`` directory (and yes, that ``@update*`` file works
even, even though it is in the "ubuntu" directory); set the AFNI
binary directory name to ``$HOME/abin/``; and add that location to the
``$PATH`` in both ``~/.cshrc`` and ``~/.bashrc``.

.. note:: if the binary package has already been downloaded, one can
          use ``-local_package``, followed by the location+name of the
          binary file, e.g.::

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
       . ~/.cshrc

   * *for* ``bash``::
   
       export R_LIBS=$HOME/R
       mkdir $R_LIBS
       echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
       . ~/.bashrc

   **Purpose:** Setup modern R from scratch. This relies on the
   environment variable ``$R_LIBS``, which specifies where to install
   the packages and where to read them from later (when R programs
   run).  

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

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst

.. 
    comment: the OLD install R stuff for this version

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
