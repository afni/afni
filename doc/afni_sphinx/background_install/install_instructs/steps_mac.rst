
.. _install_steps_mac:

*The essential system setup for:* **Mac OS**
============================================


Here we describe a complete AFNI installation and system setup for Mac
versions that are reasonably modern, such as **Mac OS 10.7+**.  The
full set of steps applies to a "clean" (i.e., empty) 10.7+ system.
There is a special step at the end for 10.11+ (El Capitan, Sierra,
etc.) users, because life is hard sometimes.

Note that 10.8 does not come with ``X11`` (or ``XQuartz``) installed.
When ``afni`` is started for the first time, you should be directed
(by the operating system) to a link to install ``XQuartz``.

.. note:: We assume that a user account exists, and that you have
          Administrator privileges, which are needed for package
          management.  That is, if you enter::
          
            sudo ls

          into a terminal and can successfully enter the password for
          the file list to display, then you are set to proceed.

0. **Initial terminal setup**

   a. *What's my shell?* Some steps below depend on what login shell
      you are running.  From a terminal window, enter::

        echo $0

      and the output should either be ``tcsh`` or ``bash``.

   #. *(optional)* Set the shell to ``tcsh``.  

      Some AFNIers prefer the syntax of ``tcsh``.

      Under System Preferences : System : Accounts menu, right-click
      on the user to get the Advanced Options menu and change the
      Login shell to ``/bin/tcsh``.

      You can verify that you have done this correctly by opening a
      new terminal and entering::
      
        echo $0
                
   #. Set the policy where "focus follows mouse", so that it is not
      necessary to first click on a new window (to select it) before
      subsequent clicks are applied to that window.  There are 3
      applications that this might apply to, so we make sure...

      From a terminal window, enter::

        defaults write org.macosforge.xquartz.X11 wm_ffm -bool true
        defaults write org.x.X11 wm_ffm -bool true
        defaults write com.apple.Terminal FocusFollowsMouse -string YES
      |

#. ``Xcode`` **and** ``XQuartz`` **installation**

   a. ``Xcode`` is needed for the gcc compiler and related tools.
      ``XQuartz`` is the desktop manager needed to run ``X11``
      programs (such as ``afni``).

      *  *For OS X 10.12*:

         a. Install ``Xcode`` using the command::

              xcode-select --install
              
         #. Install ``XQuartz`` using the "Quick Download" DMG from
            http://www.xquartz.org

      *  *For OS X 10.9 through 10.11*:

         a. Run the two commands::

              xcode-select --install
              /Applications/Utilities/X11.app

      *  *For OS X <=10.8*:

         a. It is best to start with the most recent version from the
            Apple website:
         
            - Go to https://developer.apple.com

            - Sign up for a login account (necessary for downloading)

            - Sign up via "Register as an Apple Developer" (it is
              free)

         #. Get the current "Command Line Tools" package (part of
            Developer Tools) and install it:

            - the current version is 4.6.2

            - installation defaults are good, to complete installation

         #. Install ``XQuartz`` using the "Quick Download" DMG from
            http://www.xquartz.org

         |

   #. ``XQuartz`` has altered the library format, so adjust
      ``DYLD_LIBRARY_PATH`` by doing either:

      * ``tcsh`` syntax::

         touch ~/.cshrc
         echo 'setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace' >> ~/.cshrc

      * ``bash`` syntax::

         touch ~/.bashrc
         echo 'export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace' >> ~/.bashrc

   |

#. **AFNI installation**

   a. Download and unpack the current binaries into your ``$HOME``
      directory, setting the directory name to ``$HOME/abin/``::

        cd
        curl -O https://afni.nimh.nih.gov/pub/dist/bin/macosx_10.7_local/@update.afni.binaries
        tcsh @update.afni.binaries -defaults

      .. note:: The ``$PATH`` in ``~/.cshrc`` and ``~/.bashrc`` will
                have been set by ``@update.afni.binaries``

      .. note:: If the AFNI binary package has already been
                downloaded, one can use ``-local_package``, followed
                by the location+name of the binary file, e.g. the
                third line in the above command could be::

                  tcsh @update.afni.binaries -local_package macosx_10.7_local.tgz -do_extras


#. **R installation**

    a. To download and install from the main R website:

       * Go to `the R page for Mac OS X
         <https://cran.r-project.org/bin/macosx>`_

       * Click on the latest package (probably R-3.2.3.pkg), and
         download/install it.

         |

    #. To install extra R packages needed by AFNI, run the following
       AFNI command::

           sudo rPkgsInstall -pkgs ALL


#. **PyQt4 installation** (via getting fink and using JDK)

    a. To download and install the Java SE (standard edition) JDK, go
       to http://www.oracle.com/technetwork/java/javase/downloads and
       click on the ``Java`` icon.

    #. To install the package manager ``fink``, execute the following,
       which gets an install script and executes it.  This takes
       perhaps 30 minutes and the user gets asked many questions
       (sorry, no way around it).  One can simply keep hitting the
       ``ENTER`` key to accept the useful defaults (**note:** you can
       respond with 'n' for the ``Xcode`` installation prompt if
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

#. **Set up AFNI/SUMA profiles automatically.**

   .. include:: substep_profiles.rst


#. **(optional) Prepare for an AFNI Bootcamp.**

   .. include:: substep_bootcamp.rst


#. **EVALUATE THE SETUP: an important and useful step in this
   process!!**

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
