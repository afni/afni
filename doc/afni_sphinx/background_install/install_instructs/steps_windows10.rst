.. _install_steps_windows10:


*The essential system setup for:*  **Bash on Ubuntu on Windows**
===================================================

**DANIEL**: from below, does this mean that the Ubuntu version is
 necessarily 16.04?  Would be useful to state.

Here we describe installation and system setup for the
recently-available "Bash on Ubuntu" for Windows 10 systems. Some
general background information information is provided `in their About
page <https://msdn.microsoft.com/en-us/commandline/wsl/about>`_.

.. note:: This capability is a very recent development on Windows
          systems, and we are just starting to really test out running
          AFNI and SUMA through it.  Right now, most major
          functionality seems to work, though in some cases graphics
          can be slow.  We would still recommend Linux or Mac systems
          for large-scale processing at this time.

At present, "copy+paste" functionality into the cmd window doesn't
seem to work properly out of the box. One can paste with copy/paste
control in the upper left window button.

While other operating systems allow for either ``tcsh`` or ``bash``
shells to be run in the terminal, this setup is presently limited to
the latter (as forewarned by its descriptive name...).

**DANIEL**: is the above true?  Only bash?

#. **Install prerequisite: "Bash on Windows."**

   Follow instructions here to install "Bash on Windows" from
   Microsoft, which are located `in their Installation Guide
   <https://msdn.microsoft.com/en-us/commandline/wsl/install_guide>`_.

   Install ``bash`` and Ubuntu on Windows by opening the command
   prompt window ("cmd" in "Ask me anything").

   Type::

     bash

   and the installation continues. Once installed, ``bash`` starts
   the Linux shell.


#. **Install prerequisite: "Xming X Server for Windows."**

   Click on the following link to start automatic download:
   `https://sourceforge.net/projects/xming/files/latest/download
   <https://sourceforge.net/projects/xming/files/latest/download>`_

   The default installation settings appear fine.  To set the DISPLAY
   properly in your run-command file, execute the following::

     echo "export DISPLAY=:0.0" >> ~/.bashrc


#. **Install AFNI.**

   * Do *this*: 

     **DANIEL**: what goes here? Unclear from your
     *description.  Just link to the AFNI install instructs for
     *Ubuntu?  All, or a specific?


   * Additionally, execute the following to install libXp and OpenMP::

       sudo apt update

       sudo apt-get install -y libxp6 libglu1-mesa-dev \
                               libxm4 build-essential




#. **Reboot.**

   Consider a 'reboot' at this point.  That would deal with
   system updates, the change in login shell, and an updated path::

      reboot

#. **Get R setup.**

   **DANIEL**: What about this?

   Install current R libraries for the group analysis programs.  This
   relies on the environment variable ``$R_LIBS``, which refers to a
   directory that will contain the R packages.  That variable should
   always be set, both to specify where to install the packages and
   where to read them from later (when running R programs).  The file
   obtained using ``curl`` contains instructions to add a more
   uptodate set of R libraries to the source list.  Therefore:

   * *for setting this variable in* ``tcsh`` 
     *(i.e., if you did* :ref:`tcsh setup, above <setup_Ubu_tcsh>`\ *)*::
      
       setenv R_LIBS $HOME/R
       mkdir $R_LIBS
       echo 'setenv R_LIBS ~/R' >> ~/.cshrc
       curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
       sudo tcsh @add_rcran_ubuntu.tcsh
       rPkgsInstall -pkgs ALL
      
   * *for setting this variable in* ``bash``::
      
       export R_LIBS=$HOME/R
       mkdir $R_LIBS
       echo 'export R_LIBS=$HOME/R' >> ~/.bashrc
       curl -O https://afni.nimh.nih.gov/pub/dist/src/scripts_src/@add_rcran_ubuntu.tcsh
       sudo tcsh @add_rcran_ubuntu.tcsh
       rPkgsInstall -pkgs ALL
      
   ..  
      In order, this has: set (i.e., defined) an environment variable
      called ``$R_LIBS`` to be a subdirectory called "R/" in the user's
      home directory; then made this directory; then written this
      information into the user's ``tcsh`` profile; gotten a file to
      update the rpository list; run that script; and finally run an
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
       ``$HOME/.cshrc`` (when using ``-do_extras``).  Verify this by
       visually checking that the same 'set path' line, above, in the
       (``tcsh``) profile::

         cat ~/.cshrc

       .. am inverting steps 5 and 6 from the original documentation,
          under the idea that hte Bootcamp material is secondary to a
          general install, which I feel should include R.

