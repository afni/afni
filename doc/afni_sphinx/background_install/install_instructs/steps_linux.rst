.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

.. _install_steps_linux:


*Complete system setup for:  (current) Linux*
=============================================


Here we describe installation and system setup for mainstream Linux
versions that are reasonably modern, such as **Fedora 21+, Red Hat
(RHEL) 7 and Ubuntu 14.04+**.

Several of the following steps are system dependent, for example due
to having different package managers, so we list parallel instructions
for each.

#. **Install prerequisite packages.**

   There are several packages and libraries that are needed to run the
   afni and shell programs, often even including ``tcsh``:
        
   * *for Ubuntu 14.04 (and higher)*::
      
      sudo apt-get install -y tcsh libxp6 xfonts-base python-qt4              \
                              r-base-dev libmotif4 libmotif-dev motif-clients \
                              gsl-bin netpbm gnome-tweak-tool libjpeg62
      sudo apt-get update

   * *for Fedora 21 (and higher)*::
      
       sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                           PyQt4 R-devel netpbm-progs gnome-tweak-tool ed
       sudo yum update -y
      
   * *for RHEL 7*::
      
       sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc       \
                           PyQt4 R-devel netpbm-progs gnome-tweak-tool ed     \
                           libpng12
       sudo yum update -y
            
   .. _setup_tcsh:
#. **Set "tcsh" to be the default shell (optional, but recommended).**

   ::

      chsh -s /usr/bin/tcsh

#. **Install AFNI.**

   The following will create a directory called ``$HOME/abin`` and
   install the AFNI binaries there.

   First, get the install script (*this* command actually works for both
   Fedora and Ubuntu systems)::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_fedora_21_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64:

   * *for Ubuntu 14.04 (and higher), as well as RHEL 7*::

       tcsh @update.afni.binaries -package linux_openmp_64

   * *for Fedora 21 (and higher)*::

       tcsh @update.afni.binaries -package linux_fedora_21_64

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
   called ``$R_LIBS`` to be a subdirectory called "R/" in the user's
   home directory; then made this directory; then written this
   information into the user's ``tcsh`` profile; and finally run an
   AFNI command to (hopefully) get all the necessary R libraries for
   the modern package.

#. **Automatically set up AFNI/SUMA profiles.**

   As noted in the :ref:`Technical notes <tech_notes_ENV>`, AFNI
   and SUMA have a lot of default settings, controlled using
   *environment variables*.  Vanilla-mode profiles with default values
   are easily installed:

   - for AFNI, copy it from the main directory of binaries::

       cp $HOME/abin/AFNI.afnirc $HOME/.afnirc

   - for SUMA, run the command::

       suma -update_env

     This makes and populates a profile called ``$HOME/.sumarc``.


   .. ---------- HERE/BELOW: copy for all installs --------------

#. **EVALUATE THE SETUP: an important and useful step in this
   process!**

   a. There is a very useful script to check on your installed AFNI
      and lots of its dependencies, such as looking for the installed
      R libraries, profiles, Python stuff, etc. You can run it

      - outputting to the screen::
       
          afni_system_check.py -check_all

      - outputting to a text file::
       
          afni_system_check.py -check_all > out.afni_system_check.txt

      which might be useful to email to your local AFNI Guru if there
      are any problems. 

   #. So, at this point, if your "system check" doesn't really give
      any errors, you're all set to go. If it *did* give some errors,
      please:

      - check the list of :ref:`known setup issues <install_error_msgs>`;

      - search on the `Message Board
        <https://afni.nimh.nih.gov/afni/community/board/>`_, and/or
        put the error into google;

      - post a question on the aforementioned `Message Board
        <https://afni.nimh.nih.gov/afni/community/board/>`_.
      |

#. **Setting up autoprompts for command line options.**

   The following is quite useful to be set up help files for
   tab-autocompletion of options as you type AFNI commands.  Run this
   command::

     apsearch -update_all_afni_help
      
   and then follow the brief instructions.

#. **Keeping up-to-date.**

   From this point onward, you can easily keep your AFNI uptodate just
   by running a single command::

     @update.afni.binaries -d

   That's it!! It will automatically download the correct latest
   version to your computer, replacing your old binaries.  You can
   always check your version by typing::

     afni -ver

   (And you should always check and report your version if you have
   questions/comments/etc.)

#. **(optional) Prepare for an AFNI Bootcamp.**

   .. warning::
      If you are preparing for an AFNI Bootcamp, then please see the
      :ref:`Bootcamp prep <install_bootcamp>` instructions on downloading
      the class data.  And have a nice day.


