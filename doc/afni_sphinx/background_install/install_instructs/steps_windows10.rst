.. _install_steps_windows10:


**Windows 10 (Windows Subsystem for Linux (WSL))**: *The essential system setup*
===========================================================================

.. contents:: :local:

Here we describe Linux installation and system setup for the **"Fall
Creators Update"** (FCU) version of Windows 10, known as the Windows
Subsystem for Linux (WSL).

More technical/background description of updates since the earlier
beta version of WSL can be found `HERE on their "What's New" page
<https://blogs.msdn.microsoft.com/commandline/2017/10/11/whats-new-in-wsl-in-windows-10-fall-creators-update/>`_.
Mainly, the installation is a lot easier now, and one can even run
``tcsh`` shells.  Yippee.

Importantly, as with other installation instructions, you are required
to have administrator privileges on your operating system. 

Install prerequisite: Getting Linux
-----------------------------------

| Follow the short number of steps to install WSL described here,
  selecting "Ubuntu" as your desired flavor of Linux: 
| `https://docs.microsoft.com/en-us/windows/wsl/install-win10
  <https://docs.microsoft.com/en-us/windows/wsl/install-win10>`_


..
    0. If you have a previous version of Bash on Ubuntu on Windows,* it
       will have to be removed.  Backup any files you want, and remove the
       earlier Linux (which is no longer supported).  You can see
       instructions for "Remove & replace" `HERE on this instruction page
       <https://blogs.msdn.microsoft.com/commandline/2017/04/11/windows-10-creators-update-whats-new-in-bashwsl-windows-console/>`_

    #. Click on: Start -> "Windows Features" -> "Enable Windows Subsystem for
       Linux". Then, reboot.

    #. Click on: Start -> "Microsoft Store".  Search for "Linux" and select 
       **C:\> Linux on Windows? Totally."** Click "Get the apps".

    #. From options of Linux type, choose "Ubuntu".  Click "Get" (twice,
       first time says to enable Windows Subsystem for Linux, as
       above). Download is 195.7 MB now from Canonical Group Limited.

       **Add** "Pin to Start" to make more accessible. 

       **Then click** "Launch". It reports "Installing. this may take a
       few minutes..."  in a terminal window.

    #. In the terminal window, you will be prompted to enter a "UNIX
       username". You may want to use one that is the same as one you use
       on a remote system or server. 

       Also give a new password and confirm it.

    Follow ubuntu 16 install instructions for AFNI. Note the copy paste commands in the shell terminal is hidden in the upper left icon menu. Click icon, edit, paste to paste these commands. This following step is not needed in new install.
    sudo add-apt-repository universe


Install prerequisite: Xming X Server for Windows
------------------------------------------------

a. | Click on the following link to start automatic download:
   | `https://sourceforge.net/projects/xming/files/latest/download
     <https://sourceforge.net/projects/xming/files/latest/download>`_
   | Use default installation settings.  

#. To set the DISPLAY properly, copy+paste the following into the
   terminal::

     echo "export DISPLAY=:0.0" >> ~/.bashrc
     echo "setenv DISPLAY :0.0" >> ~/.cshrc

.. note:: When you start up your WSL, you will need to double-click on
          the Xming icon on your Desktop in order to start the X
          Server.  (Sorry, not our fault...)

Install prerequisite: AFNI and  package dependencies
----------------------------------------------------

We assume your version of Ubuntu is 16.04, and so you should follow
the following setup instructions through "Make AFNI/SUMA profiles":
  
.. list-table:: 
   :header-rows: 0
   :widths: 80

   * - :ref:`Link to Ubuntu 16+ setup instructions for AFNI <install_steps_linux_ubuntu16>`

**... while noting the following:**

* The *R* package installation part might be slow, on the order of
  hours.  Meditation is often a good thing, anyways.

* Include the optional ``gnome-terminal`` installation in the first
  set of steps.

* Then, copy+paste the following into the terminal::

    sudo sed -i 's/<listen>.*<\/listen>/<listen>tcp:host=localhost,port=0<\/listen>/' /etc/dbus-1/session.conf


Useful setup tips (optional, but recommended)
---------------------------------------------

a. Install Ubuntu terminal fonts as described `HERE
   <https://www.howtogeek.com/249966/how-to-install-and-use-the-linux-bash-shell-on-windows-10/>`_.

#. Follow these ``gnome-terminal`` steps:

   * The default profile "use colors from system theme" shows an
     all-black terminal.  To fix this: select the ``Edit`` tab, then
     ``Profile``, turn **off** "use colors ...", and finally simply
     pick a scheme+palette that you like.

   * In gnome-terminal, everything is standard and similar to other
     linux implementations, and the middle button pastes whatever is
     selected in the WSL terminal or other gnome-terminal.
     Shift-Ctrl-c copies, and Shift-Ctrl-v also pastes.

.. 
    #. Like most Linux systems, some things have to be done with ``sudo``
       permissions. The username and password may have nothing to do with
       their Windows login. To reset the password for user
       ``USER_X``, follow these instructions:

       * From the default command window, type ``Super[windowskey]+X``,
         then ``A``.  You can change the default user to root::

           lxrun /setdefaultuser root

       * Now BoUoW logs you in as root without asking password. To change
         the user password::

           passwd USER_X

       * Change the default user back to your normal user in Windows
         command prompt::

           lxrun /setdefaultuser USER_X


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

.. include:: substep_rcfiles.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst



.. figure:: media/AFNI_on_Windows10_2ways.jpg
   :align: center
   :figwidth: 70%
   :name: media/AFNI_on_Windows10_2ways.jpg
   


