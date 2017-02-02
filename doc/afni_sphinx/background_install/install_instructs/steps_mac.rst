
.. _install_steps_mac:

*The essential system setup for:* **Mac OS**
============================================


Here we describe a complete AFNI installation and system setup for Mac
versions that are reasonably modern, such as **Mac OS 10.7+**.  The
full set of steps applies to a "clean" (i.e., empty) 10.7+ system.
There is a special step at the end for 10.11+ (El Capitan, Sierra,
etc.) users, because life is hard sometimes.

Note that 10.8 does not come with X11 (or XQuartz) installed.  When
afni is started for the first time, you should be directed (by the
operating system) to a link to install XQuartz.

0. **Account setup**

   Take note of your current login shell.  From a "Terminal" window, enter::

       echo $0


   Assuming a user account exists, these steps are all optional:

   a. Create a user account with ``su`` (Administrator) privileges
      (via "System Preferences", under "Accounts").

      .. note:: Admin privileges are needed for package management.

   #. (optional) Set the shell to ``/bin/tcsh``.  NB: this no longer
      works using the ``chsh ...`` command.

      Under System Preferences : System : Accounts menu, right-click
      on the user to get the Advanced Options menu and change the
      Login shell to ``/bin/tcsh``.

   #. (optional) Under System Preferences : Sharing : Services, enable
      "Remote Login" to allow ``ssh`` access.

   #. Set the policy where "focus follows mouse", so that it is not
      necessary to first click on a new window (to select it) before
      subsequent clicks are applied to that window.  There are 3
      applications that this might apply to, so we make sure...

      From a terminal window, enter::

        defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
        defaults write org.x.X11 wm_ffm -bool true
        defaults write com.apple.Terminal FocusFollowsMouse -string YES
      |

#. **Xcode and XQuartz installation**

   Xcode is needed for the gcc compiler and related tools.  XQuartz is
   the desktop manager needed to run X11 programs (such as afni).

   *  *For OS X 10.12*:

      a. install XCode using the command::

         xcode-select --install

      b. then install XQuartz from https://www.xquartz.org

   *  *For OS X 10.9 through 10.11*, simply run the 2 commands::

         xcode-select --install
         /Applications/Utilities/X11.app

   *  *Otherwise (for OS X versions up through 10.8)*, it is best to start
      with the most recent version from the Apple website:

      a. Go to https://developer.apple.com

         * Sign up for a login account (necessary for downloading) 

         * Sign up via "Register as an Apple Developer" (it is free)

      #. Get the current "Command Line Tools" package (part of Developer
         Tools) and install it

         * the current version is 4.6.2

         * installation defaults are good, to complete installation

      #. Install XQuartz using the "Quick Download" of the DMG file
         located at http://www.xquartz.org

   #. XQuartz has altered the library format, so adjust DYLD_LIBRARY_PATH.

      - *tcsh syntax*::

         touch .cshrc
         echo 'setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace' >> ~/.cshrc

      - *bash syntax*::

         touch .bashrc
         echo 'export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace' >> ~/.bashrc

   |

#. **AFNI installation**

   a. Download and unpack the current binaries into your ``$HOME``
      directory, setting the directory name to ``$HOME/abin/``::

        cd
        curl -O https://afni.nimh.nih.gov/pub/dist/bin/macosx_10.7_local/@update.afni.binaries
        tcsh @update.afni.binaries -defaults

      .. note:: ``$PATH`` in ``~/.cshrc`` and ``~/.bashrc`` was set by ``@update.afni.binaries``

      .. note:: if the binary package has already been downloaded, one
                can use ``-local_package``, followed by the
                location+name of the binary file, e.g.::

                  tcsh @update.afni.binaries -local_package macosx_10.7_local.tgz -do_extras


#. **R installation**

    a. Download and install from the main R website:

       * Go to `the R page for Mac OS X
         <https://cran.r-project.org/bin/macosx>`_

       * Click on the latest package (probably R-3.2.3.pkg), and
         download/install it.

    #. Install extra packages needed by AFNI.

       Run the following AFNI command::

           sudo rPkgsInstall -pkgs ALL


#. **PyQt4 installation** via fink and requiring JDK

    a. Download and install the Java SE (standard edition) JDK.

       * download the JDK from http://www.oracle.com/technetwork/java/javase/downloads

    #. Install fink.

       Download and run the install script.::

           curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/save/install.fink.bash
           bash install.fink.bash

    #. Install PyQt4.
       First, open a new terminal window (or source ~/.cshrc) to get fink in path.

       * in new window, verify that fink is ready::

           fink --version

       * then install PyQt4::

           sudo fink install pyqt4-mac-py27
           sudo ln -s /sw/bin/python2.7 /sw/bin/python
           echo 'setenv PYTHONPATH /sw/lib/qt4-mac/lib/python2.7/site-packages' >> ~/.cshrc


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




.. comment

   #. **Setting up autoprompts for command line options.**

   The following is quite useful to be set up help files for
   tab-autocompletion of options as you type AFNI commands.  Run this
   command::

     apsearch -update_all_afni_help
      
   and then follow the brief instructions.
