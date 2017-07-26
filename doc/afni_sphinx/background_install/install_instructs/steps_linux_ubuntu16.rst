.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux_ubuntu16:


**Ubuntu Linux (16.04 and higher)**: *The essential system setup*
===================================================

Here we describe installation and system setup for modern Ubuntu Linux
versions, specifically 16.04 (Xenial Xerus).  It *may* work for later
systems, as well.  For earlier versions of Ubuntu, please see the
relevant :ref:`instructions here <install_steps_linux_ubuntu>`.

#. **Install prerequisite packages.**

   There are several packages and libraries that are needed to run the
   afni and shell programs, often even including ``tcsh``:
     
   * *Required*::

       sudo add-apt-repository universe
       sudo apt-get update

       sudo apt-get install -y tcsh xfonts-base python-qt4       \
                               gsl-bin netpbm gnome-tweak-tool   \
                               libjpeg62 xvfb xterm vim curl
      
       sudo apt-get install -y libglu1-mesa-dev libglw1-mesa     \
                               libxm4 build-essential

     * *Optional, but may be quite useful (esp. if you are following
       the* :ref:`BoUoW <install_steps_windows10>` *instructions). The
       following may take a little while for your system to setup, but
       it should provide nice terminal behavior*::

         sudo apt-get install -y gnome-terminal
         sudo apt-get install -y nautilus
         sudo apt-get install -y gnome-icon-theme-symbolic

   .. internal note/comment: at this moment, we are eschewing
      including 'mwm', which is the replacement for 'motif-clients'
      from earlier Ubuntu versions.  If problems arise, that might be
      useful

   .. _setup_Ubu16_tcsh:
#. **(optional, but recommended) Set "tcsh" to be the default shell.**

   ::

      chsh -s /usr/bin/tcsh

#. **Install AFNI.**

   The following will create a directory called ``$HOME/abin`` and
   install the AFNI binaries there.

   First, get the install script::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_ubuntu_16_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64::

     tcsh @update.afni.binaries -package linux_ubuntu_16_64  -do_extras

   .. note:: If the binary package has already been downloaded, one
             can use ``-local_package``, followed by the location+name
             of the binary file, e.g.::

               tcsh @update.afni.binaries -local_package linux_ubuntu_16_64.tgz -do_extras

#. **Reboot.**

   Consider a 'reboot' at this point.  That would deal with
   system updates, the change in login shell, and an updated path::

      reboot

#. **Get R setup.**

   Install current R libraries for the group analysis programs.  This
   relies on the environment variable ``$R_LIBS``, which refers to a
   directory that will contain the R packages.  That variable should
   always be set, both to specify where to install the packages and
   where to read them from later (when running R programs).  The file
   obtained using ``curl`` contains instructions to add a more
   uptodate set of R libraries to the source list.  Therefore:

   * *for setting this variable in* ``tcsh`` 
     *(i.e., if you did* :ref:`tcsh setup, above <setup_Ubu16_tcsh>`\ *)*::
      
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

   Also, consider running ``gnome-tweak-tool`` and changing
   ``Windows`` -> ``Focus Mode`` from 'click' to 'mouse'.

   Also, consider extending time for screen saver:
   ``System Settings`` -> ``Brightness & Lock``, and set inactivity duration.

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
       | ``Precompiled binary linux_ubuntu_16_64: 
         Jun 16 2017 (Version AFNI_17.1.11)``


       NB: ``@update.afni.binaries`` should have set the path in
       ``$HOME/.cshrc`` (when using ``-do_extras``).  Verify this by
       visually checking that the same 'set path' line, above, in the
       (``tcsh``) profile::

         cat ~/.cshrc

       .. am inverting steps 5 and 6 from the original documentation,
          under the idea that hte Bootcamp material is secondary to a
          general install, which I feel should include R.

