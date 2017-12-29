
.. _install_steps_mac:


**Mac OS**: *The essential system setup*
========================================

.. contents:: :local:

What to do?
-----------

Here we describe a complete AFNI installation and system setup for
clean/empty Mac OS versions **10.9+**.  Each step involves either:

A. copying+pasting commands (from the green fields) into the terminal, or

#. clicking on a link to download a package.

Some steps depend on the Mac OS version. 

We assume the user has admin privileges (can run ``sudo ...``), and
some steps require an internet connection.

If you want to know what shell you are using (e.g., ``bash`` or
``tcsh``), type::

  echo $0

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

Copy+paste the following::

  cd
  curl -O https://afni.nimh.nih.gov/pub/dist/bin/macosx_10.7_local/@update.afni.binaries
  tcsh @update.afni.binaries -defaults

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

Install R
---------

a. | Click on this link: https://cran.r-project.org/bin/macosx
   | and then click on the top/latest package to install.

#. Then, copy+paste::

     sudo rPkgsInstall -pkgs ALL

   **Purpose:** This installs specific R packages needed by AFNI.


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

#. Test your PyQt4.

   Copy+paste the following::

     uber_subject.py

   Does a GUI open?  Or is there a crash??
  
   |


