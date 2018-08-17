
.. _install_steps_mac_adminR:


**Mac OS (admin'ed)**: *regular user setup*
============================================

.. contents:: :local:

What to do?
-----------

Here we describe the regular users's part of the AFNI installation and
system setup for *administered* Mac OS versions **10.9+**.  Root
privilege **is not** required.

These are accompanied by :ref:`instructions for administrators
<install_steps_mac_adminA>`, which need to be performed first.


Check shell
-----------

To find out what shell you are using (e.g., ``bash`` or ``tcsh``),
type::

  echo $0

Most AFNI scripts are written in ``tcsh``, and most command line
examples presented also use ``tcsh`` syntax.  If you would like to
change your default shell, please ask your administrator to do so (as
it typically requires admin privileges on a Mac to do so).

Setup terminal
--------------

Copy+paste the following into a terminal::

  defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
  defaults write org.x.X11 wm_ffm -bool true
  defaults write com.apple.Terminal FocusFollowsMouse -string YES

**Purpose:** This sets the policy where "focus follows mouse", so
that it is not necessary to first click on a new window (to select
it) before subsequent clicks are applied to that window.  These
commands set the policy for the 3 applications that this might
apply to.

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

Setup environment variables
---------------------------

Copy+paste the following::

  touch ~/.cshrc
  echo 'setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace' >> ~/.cshrc

  touch ~/.bashrc
  echo 'export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace' >> ~/.bashrc

**Purpose:** This adjusts the library format variable for XQuartz
in both ``tcsh`` and ``bash``.  Sigh.


Reboot
------

Please logout and log back into your system.

**Purpose:** This deals with system updates, any change in login
shell, and path updates.

.. ---------- HERE/BELOW: copy for all installs --------------

Make AFNI/SUMA profiles
-----------------------

.. include:: ../install_instructs/substep_profiles.rst

Prepare for Bootcamp
------------------------------------

.. include:: ../install_instructs/substep_bootcamp.rst

Evaluate setup/system (**important!**)
----------------------------------

.. include:: ../install_instructs/substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: ../install_instructs/substep_rcfiles_mac.rst

Keep up-to-date (remember!)
---------------------------

.. include:: ../install_instructs/substep_update.rst

|

.. comment out

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


