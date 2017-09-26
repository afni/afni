.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_Fed_RH:


**Fedora and Red Hat Linux**: *The essential system setup*
==========================================================

.. contents:: :local:

Here we describe installation and system setup for reasonably modern
Linux versions of Fedora (21+) and Red Hat (RHEL) 7, along with the
corresponding CentOS 7.

Several of the following steps are system dependent, for example due
to having different package managers, so we list parallel instructions
for each.

Install prerequisite packages
-----------------------------

Install necessary packages and libraries (this is the only step that
requires sudo ability):

* *for Fedora 21 (and higher)*::
   
    sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                        PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                        xorg-x11-server-Xvfb
    sudo yum update -y
   
* *for CentOS/RHEL 7*::

    sudo yum install -y epel-release
    sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                        PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                        libpng12 xorg-x11-server-Xvfb
    sudo yum update -y
         
.. _setup_FRH_tcsh:
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
``$HOME`` directory (and yes, that ``@update*`` file works even, even
though it is in the "ubuntu" directory); set the AFNI binary directory
name to ``$HOME/abin/``; and add that location to the ``$PATH`` in
both ``~/.cshrc`` and ``~/.bashrc``.

.. note:: if the binary package has already been downloaded, one can
          use ``-local_package``, followed by the location+name of the
          binary file, e.g.::

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
   rPkgsInstall -pkgs ALL
   
* *for* ``bash``::
   
    export R_LIBS=$HOME/R
    mkdir $R_LIBS
    echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
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
-----------------------------------

.. include:: substep_evaluate.rst


Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst

