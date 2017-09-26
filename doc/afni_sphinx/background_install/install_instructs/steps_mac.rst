
.. _install_steps_mac:


**Mac OS**: *The essential system setup*
========================================

.. contents:: :local:
   :numbered

Here we describe a complete AFNI installation and system setup for Mac
versions that are reasonably modern, such as **Mac OS 10.7+**.  The
full set of steps applies to a "clean" (i.e., empty) 10.7+ system.
Some steps are dependent on which version of Mac is present.

Note that 10.8 does not come with X11 (or XQuartz) installed.
When ``afni`` is started for the first time, you should be directed
(by the operating system) to a link to install XQuartz.

.. note:: In each step below, users can copy+paste the contents of the
          green fields into the terminal directly, i.e.,::
          
            echo " You can copy and paste me!"

Setup terminal
--------------

a. *Do I have Admin privileges?*  To find out, type::
          
     sudo ls

     If you can provide the correct password for the file list to
     display, you should be OK to proceed.

#. *What's my shell?*  To find out, type::

     echo $0

   and the output should either be ``tcsh`` or ``bash`` (possibly
   with a ``-`` in front).  That's your current shell.

#. *(optional)* Some AFNIers prefer the syntax of ``tcsh``.  To set
   this as default, go to System Preferences : System : Accounts menu,
   right-click on the user to get the Advanced Options menu and change
   the Login shell to ``/bin/tcsh``.

   You can verify that you have done this correctly by opening a
   new terminal and entering::
      
     echo $0
                
#. Set the policy where "focus follows mouse", so that it is not
   necessary to first click on a new window (to select it) before
   subsequent clicks are applied to that window.  There are 3
   applications that this might apply to, so enter the following in a
   terminal::

     defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
     defaults write org.x.X11 wm_ffm -bool true
     defaults write com.apple.Terminal FocusFollowsMouse -string YES


Install Xcode and XQuartz
-------------------------

a. Xcode is needed for the gcc compiler and related tools.
   XQuartz is the desktop manager needed to run X11
   programs (such as ``afni``).

   *  *For OS X >= 10.11,* install Xcode using the command::

           xcode-select --install
           
      **and** install XQuartz using the "Quick Download" DMG from
      http://www.xquartz.org

   *  *For OS X 10.9 and 10.10,* run the two commands::

           xcode-select --install
           /Applications/Utilities/X11.app

   *  *For OS X <=10.8,*

      i. Start with the most recent version from the Apple website:
      
         - Go to https://developer.apple.com

         - Sign up for a login account (necessary for downloading)

         - Sign up via "Register as an Apple Developer" (it is
           free)

      #. Get the current "Command Line Tools" package (part of
         Developer Tools) and install it:

         - the current version is 4.6.2

         - installation defaults are good, to complete installation

      #. Install XQuartz using the "Quick Download" DMG from
         http://www.xquartz.org

   |

#. Run the following::

     touch ~/.cshrc
     echo 'setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace' >> ~/.cshrc

     touch ~/.bashrc
     echo 'export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace' >> ~/.bashrc

   This adjusts the library format variable for XQuartz in both
   ``tcsh`` and ``bash``.  Sigh.


Install AFNI binaries
---------------------

Run the following::

  cd
  curl -O https://afni.nimh.nih.gov/pub/dist/bin/macosx_10.7_local/@update.afni.binaries
  tcsh @update.afni.binaries -defaults

These commands: download and unpack the current binaries into your
``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the AFNI binary package has already been downloaded, one
          can use ``-local_package``, followed by the location+name of
          the binary file, e.g. the third line in the above command
          could be::

            tcsh @update.afni.binaries -local_package macosx_10.7_local.tgz -do_extras


Install R
---------

a. | Go to the main R page for Mac OS X: `HERE <https://cran.r-project.org/bin/macosx>`_,
   | and click on the latest package (probably R-3.4.0.pkg) to
     download+install it.

#. Then, install extra R packages specifically needed by AFNI::

     sudo rPkgsInstall -pkgs ALL


Install PyQt4, via JDK and fink (optional)
------------------------------------------

*NB: no longer necessary for the Bootcamp!*

a. To download and install the Java SE (standard edition) JDK, go
   to http://www.oracle.com/technetwork/java/javase/downloads and
   click on the ``Java`` icon.

#. To install the package manager ``fink``, execute the following,
   which gets an install script and executes it.  This takes
   perhaps 30 minutes and the user gets asked many questions
   (sorry, no way around it).  One can simply keep hitting the
   ``ENTER`` key to accept the useful defaults (**note:** you can
   respond with 'n' for the Xcode installation prompt if
   prompted otherwise, as you should have it from an earlier
   step).

   Run the commands::

       curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/save/install.fink.bash
       bash install.fink.bash

#. Install PyQt4.

   First, open a new terminal window (or ``source ~/.cshrc`` or
   ``source ~/.bashrc``) to make sure that ``fink`` has been added
   to the ``$PATH``.

   * In a new window, verify that fink is ready::

       fink --version

   * Run the following to install PyQt4::

       sudo fink install pyqt4-mac-py27
       sudo ln -s /sw/bin/python2.7 /sw/bin/python
       echo 'setenv PYTHONPATH /sw/lib/qt4-mac/lib/python2.7/site-packages' >> ~/.cshrc

     (You likely won't get a 'success' message here, but you can
     use the **Evaluate** step below to verify the installation.)

   |

.. ---------- HERE/BELOW: copy for all installs --------------

Make AFNI/SUMA profiles
-----------------------

.. include:: substep_profiles.rst

Prepare for Bootcamp (semi-optional)
------------------------------------

.. include:: substep_bootcamp.rst

Evaluate setup/system (important!)
----------------------------------

.. include:: substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles_mac.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst


