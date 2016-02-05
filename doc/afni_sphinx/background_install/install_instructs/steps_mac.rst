
.. _install_steps_mac:

**Complete system setup for:  (modern) Mac OS**
===============================================


Here we describe a complete AFNI installation and system setup for Mac
versions that are reasonably modern, such as:

* Mac OS 10.7+

The full set of steps applies to a "clean" (i.e., empty) 10.7+ system.
There is a special step at the end for 10.11 (El Capitan) users,
because life is hard sometimes.

Note that 10.8 does not come with X11 (or XQuartz) installed.  When
afni is started for the first time, you should be directed (by the
operating system) to a link to install XQuartz.

0. **Account setup**

   Assuming a user account exists, these steps are all optional:

   a. Create a user account with ``su`` (Administrator) privileges
      (via "System Preferences", under "Accounts").

      .. note:: Admin privileges are needed for package management.

   #. (optional) Set the shell to ``/bin/tcsh``.  This no longer works
      using the ``chsh ...`` command.

      Under System Preferences : System : Accounts menu, right-click
      on the user to get the Advanced Options menu and change the
      Login shell to ``/bin/tcsh`.

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

#. **Xcode installation**

   Xcode is needed for the gcc compiler and related tools. It is best
   to start with the most recent version from the Apple website:

   a. Go to http://developer.apple.com

      * Sign up for a login account (necessary for downloading) 

      * Sign up via "Register as an Apple Developer" (it is free)

   #. Get the current "Command Line Tools" package (part of Developer
      Tools) and install

      * current version is 4.6.2

      * installation defaults are good, to complete installation
   |

#. **Homebrew installation**

   At this point, we will install the :ref:`package manager
   <tech_notes_PacMan>` Homebrew:

   a. Install HomeBrew and Python
 
      * Type this command to run the Homebrew installation script,
        choosing one of these :ref:`shell <tech_notes_PacMan>`
        syntaxes:

        - *for tcsh*::

           curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install | ruby

        - *for bash*::

           ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
                    

   #. Make sure the Homebrew installation succeeded with no errors by
      typing this command::

        brew doctor

   #. Install PyQt4, enabling use of the uber_*.py programs::

        brew install pyqt



#. **AFNI installation**

   a. Download and unpack the current binaries into your ``$HOME``
      directory, changing the directory name to ``$HOME/abin/``::

        cd
        curl -O http://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.7_Intel_64.tgz
        tar -xzf macosx_10.7_Intel_64.tgz
        mv macosx_10.7_Intel_64 abin
        rm macosx_10.7_Intel_64.tgz

   #. Update the path and library path.

      * *for tcsh*::

          echo 'set path = (/usr/local/bin $path $HOME/abin )' >> .cshrc
          echo 'setenv DYLD_FALLBACK_LIBRARY_PATH $HOME/abin' >> .cshrc
          echo 'setenv PYTHONPATH /usr/local/lib/python2.7/site-packages' >> .cshrc
          source .cshrc
          rehash

      * *for bash*::

          echo 'export PATH=/usr/local/bin:$PATH:$HOME/abin' >> .profile
          echo 'export DYLD_FALLBACK_LIBRARY_PATH=$HOME/abin' >> .profile
          echo 'export PYTHONPATH=/usr/local/lib/python2.7/site-packages' >> .profile
          . .profile

#. **AFNI verification**
   
   a. Try it out!  If these start, you should be in good shape::

          afni
          suma
          
      .. note:: For 10.8 users, when prompted, follow the instructions
                to install XQuartz.

   #. ...?? profiles... ... system check ...   R??

   #. The following is quite useful to be set up help files for
      tab-autocompletion of options as you type AFNI commands.  Run
      this command::

          apsearch -update_all_afni_help
      
      and then follow the brief instructions.


    4f. Get sample data and review AFNI handouts.


?? getting R?  `R for Mac OS X <https://cran.r-project.org/bin/macosx>`_





|

|

:Date: |today|
