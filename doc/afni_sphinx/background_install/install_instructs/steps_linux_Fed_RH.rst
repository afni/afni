.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_Fed_RH:


*The essential system setup for:* **Fedora and Red Hat Linux**
==============================================================


Here we describe installation and system setup for reasonably modern
Linux versions of Fedora (21+) and Red Hat (RHEL) 7.

Several of the following steps are system dependent, for example due
to having different package managers, so we list parallel instructions
for each.

#. **Install prerequisite packages.**

   There are several packages and libraries that are needed to run the
   afni and shell programs, often even including ``tcsh``:

   * *for Fedora 21 (and higher)*::
      
       sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                           PyQt4 R-devel netpbm-progs gnome-tweak-tool ed
       sudo yum update -y
      
   * *for RHEL 7*::
      
       sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                           PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                           libpng12
       sudo yum update -y
            
   .. _setup_FRH_tcsh:
#. **(optional, but recommended) Set "tcsh" to be the default shell.**

   ::

      chsh -s /usr/bin/tcsh

#. **Install AFNI.**

   The following will create a directory called ``$HOME/abin`` and
   install the AFNI binaries there.

   First, get the install script (*this* command actually works for both
   Fedora and RHEL systems)::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_fedora_21_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64:

   * *for RHEL 7*::

       tcsh @update.afni.binaries -package linux_openmp_64

   * *for Fedora 21 (and higher)*::

       tcsh @update.afni.binaries -package linux_fedora_21_64

#. **Reboot.**

   Consider a 'reboot' at this point.  That would deal with
   system updates, the change in login shell, and an updated path::

      reboot

#. **Get R setup.**

   Install current R libraries for the group analysis programs.  This
   relies on the environment variable ``$R_LIBS``, which refers to a
   directory that will contain the R packages.  That variable should
   always be set, both to specify where to install the packages and
   where to read them from later (when running R programs).
   Therefore:
      
   * *for setting this variable in* ``tcsh`` 
     *(i.e., if you did* :ref:`tcsh setup, above <setup_FRH_tcsh>`\ *)*::

      setenv R_LIBS $HOME/R
      mkdir $R_LIBS
      echo 'setenv R_LIBS ~/R' >> ~/.cshrc
      rPkgsInstall -pkgs ALL
      
   * *for setting this variable in* ``bash``::
      
       export R_LIBS=$HOME/R
       mkdir $R_LIBS
       echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
       rPkgsInstall -pkgs ALL

   ..
     In order, this has: set (i.e., defined) an environment variable
     called ``$R_LIBS`` to be a subdirectory called "R/" in the user's
     home directory; then made this directory; then written this
     information into the user's ``tcsh`` profile; and finally run an
     AFNI command to (hopefully) get all the necessary R libraries for
     the modern package.


   .. ---------- HERE/BELOW: copy for all installs --------------

#. **Automatically set up AFNI/SUMA profiles.**

   .. include:: substep_profiles.rst

#. **(optional) Prepare for an AFNI Bootcamp.**

   .. include:: substep_bootcamp.rst


#. **EVALUATE THE SETUP: an important and useful step in this
   process!**

   .. include:: substep_evaluate.rst


#. **(optional) Niceifying interfaces: it's a magical terminal.**

   .. include:: substep_rcfiles.rst


#. **Keeping up-to-date (remember).**

   .. include:: substep_update.rst




.. commented out-- older steps, unnecessary here.

   #. **Setting up autoprompts for command line options.**

   The following is quite useful to be set up help files for
   tab-autocompletion of options as you type AFNI commands.  Run this
   command::

     apsearch -update_all_afni_help
      
   and then follow the brief instructions.



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

       | The final command should show something useful, like:
       | ``Precompiled binary linux_ubuntu_12_64: 
         Feb 29 2016 (Version AFNI_16.0.10)``


       NB: ``@update.afni.binaries`` should have set the path in
       ``$HOME/.cshrc``.  Verify this by visually checking that the same
       'set path' line, above, in the (``tcsh``) profile::

         cat ~/.cshrc

       .. am inverting steps 5 and 6 from the original documentation,
          under the idea that hte Bootcamp material is secondary to a
          general install, which I feel should include R.

