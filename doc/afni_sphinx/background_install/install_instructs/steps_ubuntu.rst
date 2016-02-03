
.. _install_steps_ubuntu:

**Complete system setup for:  (current) Linux**
===============================================

.. contents::
   :depth: 3

.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

Here we describe installation and system setup for mainstream Linux
versions that are reasonably modern, such as:

* Fedora 21+
* Ubuntu 14.04+

Several of the following steps are system dependent, for example due
to having different package managers, so we list parallel instructions
for each.

1. **Install supplementary packages.**

   There are several packages and libraries that are needed to run the
    afni and shell programs. Note that ``tcsh`` might not be on the
    system yet, so we install it also:
        
   * for Fedora 21 (and higher)::
      
      sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc   \
                          PyQt4 R-devel netpbm-progs gnome-tweak-tool ed
      sudo yum update -y
      
   * for Ubuntu 14.04 (and higher)::
      
      sudo apt-get install -y tcsh libxp6 xfonts-base python-qt4              \
                              r-base-dev libmotif4 libmotif-dev motif-clients \
                              gsl-bin netpbm gnome-tweak-tool libjpeg62
      sudo apt-get update
      
   Each command basically goes through a list of known packages to install,
   along with any dependencies.  By default, the package manager will ask the
   user to verify steps with yes/no questions, and the ``-y`` option will
   automatically answer "yes" to every prompt, to simplify the user's life.

#. **Set ``tcsh`` to be the default shell (optional).**

   Now that ``tcsh`` is installed, set it as the default shell (if
   desired). Some subsequent instructions assume this shell.  Also,
   many Message Board postings and scripts in demos, which may be
   useful for reference, are written in ``tcsh``.  So, the choice is
   yours, but choose wisely...::

      chsh -s /usr/bin/tcsh

#. **Install AFNI.**

   Assuming there is nothing yet on the system, the following command
   will create a directory called ``$HOME/abin`` and install the AFNI
   binaries there.  It will also update the ``$path`` variable in the
   shell profile (e.g., ``$HOME/.cshrc`` for ``tcsh``), so that the
   system knows to look there for commands to execute, and it will set
   up AFNI command tabbing.

   First, get the install script (*this* command actually works for both
   Fedora and Ubuntu systems)::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_fedora_21_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64:

   * for Fedora 21 (and higher)::

       tcsh @update.afni.binaries -package linux_fedora_21_64

   * for Ubuntu 14.04 (and higher)::

       tcsh @update.afni.binaries -package linux_openmp_64

#. **Reboot.**

   Consider a 'reboot' at this point.  That would deal with
   system updates, the change in login shell, and an updated path::

      reboot

#. **Quick test.**

   Do a quick test to see that afni works::

      afni -ver

   If this doesn't produce anything constructive immediately, or if
   ``reboot`` was skipped, try starting a new ``tcsh`` shell (e.g., by
   opening a new terminal) and updating the path (again, specifically
   for ``tcsh``)::
      
      tcsh
      set path = ( $path ~/abin )
      rehash
      afni -ver
      
   The final command should show something useful.

   NB: ``@update.afni.binaries`` should have set the path in
   ``$HOME/.cshrc``.  Verify this by visually checking that the same
   'set path' line, above, in the (``tcsh``) profile::

     cat ~/.cshrc

   .. am inverting steps 5 and 6 from the original documentation,
      under the idea that hte Bootcamp material is secondary to a
      general install, which I feel should include R.


#. **Get R setup.**

   Install current R libraries for the group analysis programs.  This
   relies on the environment variable ``$R_LIBS``, which refers to a
   directory that will contain the R packages.  That variable should
   always be set, both to specify where to install the packages and
   where to read them from later (when running R programs).  For
   setting this variable in ``tcsh``, the following commands would be
   run::
      
      setenv R_LIBS $HOME/R
      mkdir $R_LIBS
      echo 'setenv R_LIBS ~/R' >> ~/.cshrc
      rPkgsInstall -pkgs ALL
      
   In order, this has: set (i.e., defined) an environment variable
   called ``R_LIBS`` to be a path of the user's home and a
   subdirectory called "R"; then made this directory; then stored this
   information in the user's profile; and finally run an AFNI command
   to (hopefully) get all the necessary R libraries for the modern
   package.

#. **Setting up AFNI/SUMA profiles.**

   As noted in the :ref:`Technical notes <install_tech_notes>`, AFNI
   and SUMA have a lot of default settings, controlled using
   *environment variables*.  It's useful to have a lot of the default
   settings in explicit profiles on the system.  AFNI and SUMA will
   look for these files each time they are run, and each profile is
   installed pretty simply (with default values):

   - for AFNI, copy it from the main directory of binaries (here, this
     is assumed to be in ``$HOME/abin/`` as the default binary
     installation described; otherwise, you can change the path in the
     first term accordingly)::

       cp $HOME/abin/AFNI.afnirc $HOME/.afnirc

   - for SUMA, just call the option to make the profile in the right
     place and to automatically copy default values there::

       suma -update_env

     This makes and populates a profile called ``$HOME/.sumarc``.


   .. _install_bootcamp:

#. **Install AFNI Bootcamp class data (optional).**

   This step may be required if you are about to attend a Bootcamp, or
   merely just useful (thar be lots of scripts and demos, accompanied
   by didactic reading material).  The Bootcamp and material in the
   data directory is discussed separately on the :ref:`Bootcamp page
   <Bootcamping>`, but we mention how it can be downloaded and
   unpacked (which is all "installation" entails) here.  By default,
   we described is installing the class data in the ``$HOME``
   directory, so that it is easy to access during a class::

      curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
      tar xvzf CD.tgz
      cd CD
      tcsh s2.cp.files . ~
      cd ..
      
   In order, these commands: get the tarred+zipped directory that
   contains the class data (and is hence named "CD"), downloading it
   to the current location in the terminal; untars/unzips it (=opens
   it up); goes into the newly opened directory; executes a script to
   copy the files to '`$HOME/CD/`'; and finally exits the directory.

   At this point, if there have been no errors, you can delete/remove
   the tarred/zipped package, using "``rm CD.tgz``".  If you are
   *really* confident, you can also deleted the CD tree in the present
   location (but leaving it in ``$HOME/CD/``).

#. **EVALUATE THE SETUP: an important and useful step in this
   process!**

   There is a very useful script to check on your installed AFNI and
   lots of its dependencies, such as looking for the installed R
   libraries, profiles, Python stuff, etc. You can run it

   - outputting to the screen::

      afni_system_check.py -check_all

   - outputting to a text file::

       afni_system_check.py -check_all > out.afni_system_check.txt

     which might be useful to email to your local AFNI Guru if there
     are any problems.
      
So, at this point, if your "system check" doesn't really give any
errors, you're all set to go. If it did give some errors, please:

- check the list of :ref:`known setup issues <install_error_msgs>`.

- search on the `Message Board
  <https://afni.nimh.nih.gov/afni/community/board/>`_, and/or put the
  error into google;

- email any questions.


|

:Date: |today|
