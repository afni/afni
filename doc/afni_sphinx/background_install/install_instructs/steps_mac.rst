
.. _install_steps_mac:


**Mac OS**: *The essential system setup*
========================================

.. contents:: :local:

What to do?
-----------

Here we describe a complete AFNI installation and system setup for
clean/empty Mac OS versions **10.9+**.

 .. include:: substep_intro.rst

.. note:: *If you are seeking the new App version of install
          instructions, please see* :ref:`HERE
          <install_steps_mac_app>`.

Setup terminal
--------------

a. Copy+paste the following into a terminal::

     defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
     defaults write org.x.X11 wm_ffm -bool true
     defaults write com.apple.Terminal FocusFollowsMouse -string YES

   **Purpose:** This sets the policy where "focus follows mouse", so
   that it is not necessary to first click on a new window (to select
   it) before subsequent clicks are applied to that window.  These
   commands set the policy for the 3 applications that this might
   apply to.

Install Xcode and XQuartz
-------------------------

a. Do the following for your system number:

   *  *For OS X >= 10.11,* 

      i. Copy+paste the following::

           xcode-select --install
           
      #. | Then, click on this link: http://www.xquartz.org 
         | and then click on the "Quick Download" DMG and 
           follow instructions to install.

   *  *For OS X 10.9 and 10.10,* 

      i. Copy+paste these two commands::

           xcode-select --install
           /Applications/Utilities/X11.app

   **Purpose:** These install Xcode, which is needed for the gcc
   compiler and related tools, and XQuartz, which is the desktop
   manager needed to run X11 programs (such as ``afni``).

#. Copy+paste the following::

     touch ~/.cshrc
     echo 'setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace' >> ~/.cshrc

     touch ~/.bashrc
     echo 'export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace' >> ~/.bashrc

   **Purpose:** This adjusts the library format variable for XQuartz
   in both ``tcsh`` and ``bash``.  Sigh.


Install AFNI binaries
---------------------

a. Copy+paste the following::

     cd
     curl -O https://afni.nimh.nih.gov/pub/dist/bin/macosx_10.7_local/@update.afni.binaries

#. Then,

   *  *For OS X >= 10.12*, copy+paste::

        tcsh @update.afni.binaries -defaults -package macos_10.12_local

   *  *For OS X < 10.12*, copy+paste::

        tcsh @update.afni.binaries -defaults -package macosx_10.7_local

**Purpose:** download and unpack the current binaries into your
``$HOME`` directory; set the AFNI binary directory name to
``$HOME/abin/``; and add that location to the ``$PATH`` in both
``~/.cshrc`` and ``~/.bashrc``.

.. note:: If the AFNI binary package has already been downloaded
          already (say, to save time/bandwidth), one can use
          ``-local_package``, followed by the location+name of the
          binary file, e.g. the third line in the above command could
          be::

            tcsh @update.afni.binaries -local_package macosx_10.7_local.tgz -do_extras

Reboot
------

Please reboot your system.

**Purpose:** This deals with system updates, any change in login
shell, and path updates.

Install R
---------

.. comment out old
  a. | Click on this link: https://cran.r-project.org/bin/macosx
     | and then click on the top/latest package to install.

a. First, click on this link to download a recent (but not the *most*
   recent) version of R:
   https://cran.r-project.org/bin/macosx/el-capitan/base/R-3.4.1.pkg

#. Then, copy+paste::

     sudo rPkgsInstall -pkgs ALL

   **Purpose:** Get specific R packages needed for AFNI programs.

Install Netpbm
--------------

.. include:: substep_netpbm.rst

.. ---------- HERE/BELOW: copy for all installs --------------

Make AFNI/SUMA profiles
-----------------------

.. include:: substep_profiles.rst

Prepare for Bootcamp
------------------------------------

.. include:: substep_bootcamp.rst

Evaluate setup/system (**important!**)
----------------------------------

.. include:: substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles_mac.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst


Install PyQt4, via JDK and fink (optional)
------------------------------------------

a. | Click on this link: http://www.oracle.com/technetwork/java/javase/downloads
   | and then click on the ``Java`` icon.

   **Purpose:** Install Java SE (standard edition) JDK.

#. Copy+paste the following::
   
     curl -O https://afni.nimh.nih.gov/pub/dist/bin/misc/save/install.fink.bash
     bash install.fink.bash

   **Purpose:** This runs an install script to download+install the
   package manager ``fink``.  

   This takes perhaps 30 minutes to finish **and** the user gets asked
   many questions (sorry, no way around it).  One can simply keep
   hitting the ``ENTER`` key to accept the useful defaults (**note:**
   you can respond with 'n' for the Xcode installation prompt if
   prompted otherwise, as you should have it from an earlier step).

#. Do each of the following (installs PyQt4):

   i. Open a new terminal window.

   #. Copy+paste::

        fink --version

   #. If no errors, copy+paste::

       sudo fink install pyqt4-mac-py27
       sudo ln -s /sw/bin/python2.7 /sw/bin/python
       echo 'setenv PYTHONPATH /sw/lib/qt4-mac/lib/python2.7/site-packages' >> ~/.cshrc

#. Test your PyQt4.

   Copy+paste the following::

     uber_subject.py

   Does a GUI open?  Or is there a crash??
  
   |


