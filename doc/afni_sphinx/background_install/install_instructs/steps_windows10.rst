.. _install_steps_windows10:


*The essential system setup for:*  **Windows 10 ('Bash on Ubuntu')**
================================================================

**INSTRUCTIONS UNDER CONSTRUCTIONS: DO NOT USE!! COULD LEAD TO
DESTRUCTIONS (well, ok, probably not, just frustructions).**

Here we describe installation and system setup for the
recently-available "Bash on Ubuntu" for Windows 10 systems. Some
general background information information is provided `in their
'About' page
<https://msdn.microsoft.com/en-us/commandline/wsl/about>`_.  At
present, the default version of Ubuntu appears to be 16.04.

.. note:: This capability is a very recent development on Windows
          systems, and we are just starting to really test out running
          AFNI and SUMA through it.  Right now, most major
          functionality seems to work, though in some cases graphics
          can be slow.  We would still recommend Linux or Mac systems
          for large-scale processing at this time.

At present, "copy+paste" functionality into the command window doesn't
seem to work properly out of the box. **CLARIFY->** One can paste with
copy/paste control in the upper left window button.

Other operating systems allow for either ``tcsh`` or ``bash`` shells
to be run as default in the terminal. *However*, the Windows-Ubuntu
presently does not appear to allow ``tcsh`` as default (perhaps as
forewarned by its descriptive name...), and therefore we describe the
system setup only for staying with ``bash``.

Importantly, as with other installation instructions, you are required
to have administrator privileges on your operating system. 

#. **Install prerequisite: "Bash on Windows."**

   * First, follow `these instructions
     <https://msdn.microsoft.com/en-us/commandline/wsl/install_guide>`_
     to install "Bash on Windows" from Microsoft. (Here, in
     particular, requires admin privileges to go into "developer
     mode.")
   
   * To install ``bash`` and Ubuntu on Windows: in the lower left of
     the Desktop, click on "Ask me anything" and type "cmd".

   * In the command window that opens up, type::

       bash

     and the installation continues. Once installed, ``bash`` starts
     the Linux shell.  

   From this point, your terminal behaves very similarly to how it would
   on a normal (non-Windows) Linux system.

   To open up a terminal, you can go to the Search bar in the lower left
   (where it says "Type here to search")
   and start typing "Bash on Ubuntu," and it will likely
   autocomplete while typing.  Additionally, you can make a shortcut
   on your Windows desktop.

#. **Install prerequisite: Xming X Server for Windows.**

   Click on the following link to start automatic download:
   `https://sourceforge.net/projects/xming/files/latest/download
   <https://sourceforge.net/projects/xming/files/latest/download>`_

   The default installation settings appear fine.  To set the DISPLAY
   properly in your run-command file, execute the following::

     echo "export DISPLAY=:0.0" >> ~/.bashrc

#. **Install prerequisite: More packages.**

   There are several packages and libraries that are needed to run the
   afni and shell programs::

     sudo apt-get install -y tcsh xfonts-base python-qt4                    \
                             libmotif4 libmotif-dev motif-clients           \
                             gsl-bin netpbm gnome-tweak-tool libjpeg62
     sudo apt-get update
     sudo apt-get install -y libxp6 libglu1-mesa-dev \
                             libxm4 build-essential

   .. note to self: 
      check on libmotif4* vs libxm4 here??

#. **Install prerequisite: GSL.**

   To install GSL::

     sudo apt-get install -y libgsl0-dev gsl-bin

   Setup link for GSL. Check the output of::

     find /usr/lib -name "libgsl*"

   If one of the things that shows up is::

     /usr/lib/x86_64-linux-gnu/libgsl.so

   Then you can enter the following::

     sudo ln -s /usr/lib/x86_64-linux-gnu/libgsl.so /usr/lib/libgsl.so.0

   And life should be good (we will validate package installations
   using an AFNI system check command, below).

#. **Install AFNI.**

   The following will create a directory called ``$HOME/abin`` and
   install the AFNI binaries there.

   First, get the install script::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_fedora_21_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64::

     tcsh @update.afni.binaries -package linux_openmp_64 -do_extras

   A comment: in trial runs, ``3dExtractGroupinCorr`` help was very,
   very slow -- it took about 10 minutes.

   .. check the above
      see if that is a general property!

   .. note:: If the binary package has already been downloaded, one
             can use ``-local_package``, followed by the location+name
             of the binary file, e.g.::
               
               tcsh @update.afni.binaries -local_package linux_openmp_64.tgz -do_extras

   .. note:: Quick AFNI test. Run the following in a terminal::
               
               afni ~/abin/

             -> that should open up the AFNI GUI with some
             template data loaded.

   .. note:: Running the uber_subject GUI should work::

               uber_subject.py

             -> there might be some terminal messages that look like
             errors, but these should be non-fatal.
              


.. probably this is unnecessary!:

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

#. **(optional) Other tips.**

   * Installation Ubuntu fonts for the terminal is described on this
     `help page
     <https://www.howtogeek.com/249966/how-to-install-and-use-the-linux-bash-shell-on-windows-10/>`_.

   * Install a different terminal than the default command window.
     Choose from one of the following.  Note that copy+paste works
     better in ``gnome-terminal`` than in either the default cmd
     window or in ``xterm``.

     #. ``xterm``.

        Execute the following::

          sudo apt-get install xterm

        You may receive errors about "cannot load font," but it still
        works after doing the following in the terminal:
        ``ctrl-right-click``, select TrueType Fonts, and change font
        size to a font you like.

     #. ``gnome-terminal``.

        The following gets virtually every other ``gnome-xxxx``
        function, too, so overall this is a pretty long
        download+install::

          sudo apt-get gnome-terminal

        This odd patch is also required::

          sudo sed -i 's/<listen>.*<\/listen>/<listen>tcp:host=localhost,port=0<\/listen>/' /etc/dbus-1/session.conf

        Then, the ``gnome-terminal`` seems to work, but the default
        profile "use colors from system theme" shows an all black
        terminal.  To fix this: select the ``Edit`` tab, then
        ``Profile``, turn **off** "use colors ...", and finally simply
        pick a scheme+palette that you like.

    #. Terminal comparison
    attached a screen capture of the three basic terminal windows.
    From left to right - 
       "Command Prompt", "Bash on Ubuntu on Windows", gnome-terminal
       (there is also the Windows PowerShell available through "Super-X,A"
       that is almost the same thing too as the "Command Prompt")
    Note the small icons in the upper left corner of each. 
    Copy/paste is available by default in the first two by left-clicking
    on the upper-left button of the window
       Select "Edit", "Mark" to select,
       then "Edit","Copy". Select "Edit","Paste" to paste - fairly awkward. 
       If you select "Properties", then "Quick Edit" to change the defaults
       of the shortcut and make this a little easier. You can then use the mouse
       to select text, "Shift-Ctrl-c" to copy and right-click with a mouse to paste.
    In gnome-terminal, everything is standard and similar to other linux implementations
       middle button pastes whatever is selected in the BouoW terminal
       or other gnome-terminal. 
       Shift-Ctrl-c copies, and Shift-Ctrl-v also pastes.

Like most linux systems, some things have to be done with sudo permissions. The user name and password may have nothing to do with their Windows login. To reset the password, follow these instructions:

    In the Windows admin command prompt (Super[windowskey]+X, A) 
    change the default user to root:
       lxrun /setdefaultuser root

    Now Bash on Ubuntu on Windows logs you in as root without asking password
    Use passwd command in Bash to change the user password:
       passwd your_username

    Change the default user back to your normal user in Windows command prompt
       lxrun /setdefaultuser your_username


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

