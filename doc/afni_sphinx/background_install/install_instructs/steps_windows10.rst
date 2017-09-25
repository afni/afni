.. _install_steps_windows10:


**Windows 10 ('Bash on Ubuntu')**: *The essential system setup*
================================================================

.. contents:: :local:

Here we describe installation and system setup for the
recently-available "Bash on Ubuntu" for Windows 10 (BoUoW)
systems. Some general background information information is provided
`in their 'About' page
<https://msdn.microsoft.com/en-us/commandline/wsl/about>`_.  At
present, the default version of Ubuntu appears to be 16.04.

.. note:: This capability is a very recent development on Windows
          systems, and we are just starting to really test out running
          AFNI and SUMA through it.  Right now, most major
          functionality seems to work, though in some cases graphics
          can be slow.  We would still recommend Linux or Mac systems
          for large-scale processing at this time.

At present, "copy+paste" functionality into the default command window
works a bit awkwardly.  One should be able to copy/paste by selecting
tabs from a drop-down panel in the upper left of the window.  (As part
of the instructions here, a terminal with nicer interface will be
installed.)

Other operating systems allow for either ``tcsh`` or ``bash`` shells
to be run as default in the terminal. *However*, the Windows-Ubuntu
presently does not appear to allow ``tcsh`` as default (perhaps as
forewarned by its descriptive name...), and therefore we describe the
system setup only for staying with ``bash``.

Importantly, as with other installation instructions, you are required
to have administrator privileges on your operating system. 

**Install prerequisite: "Bash on Windows"**
---------------------------------

* Follow the instructions in the first paragraph `HERE
  <https://technet.microsoft.com/en-us/library/ff629472.aspx>`_ to get
  Windows PowerShell up and running.

* Follow the instructions `HERE
  <https://msdn.microsoft.com/en-us/commandline/wsl/install_guide>`_
  to install "Bash on Windows" from Microsoft. (Requires Admin
  privileges.)

* Install ``bash`` and Ubuntu on Windows: in the lower left of the
  Desktop, click on "Ask me anything" and type::

    cmd

* In the command window that opens up, type::

    bash

  and the installation continues. Once installed, ``bash`` starts
  the Linux shell.  

From this point, standard Linux system terminal commands should
work (e.g., ``ls``, ``cp``, etc.).

To open up a terminal, you can go to the Search bar in the lower left
(where it says "Type here to search") and start typing "Bash on
Ubuntu"-- it will likely autocomplete while typing.  Additionally, you
can make a shortcut on your Windows desktop.

**Install prerequisite: Xming X Server for Windows**
---------------------------------

| Click on the following link to start automatic download:
| `https://sourceforge.net/projects/xming/files/latest/download
  <https://sourceforge.net/projects/xming/files/latest/download>`_

Use default installation settings.  

To set the DISPLAY properly, run::

  echo "export DISPLAY=:0.0" >> ~/.bashrc

.. note:: When you start up your BoUoW, you will need to double-click
          on the Xming icon in order to start the X Server.  (Sorry,
          not our fault...)

**Install prerequisite: AFNI and basic Linux package dependencies.**
------------------------------------------------------------------

We assume your version of Ubuntu is 16.04, and so you should, you
should now follow the following setup instructions through "Make
AFNI/SUMA profiles":
  
.. list-table:: 
   :header-rows: 0
   :widths: 80

   * - :ref:`Link to Ubuntu 16+ setup instructions for AFNI <install_steps_linux_ubuntu16>`

**... while noting the following:**

* Again, at present it does not appear possible to change shells to
  ``tcsh``, so that optional step can be skipped.

* The *R* package installation part might be slow, on the order of
  hours.  Meditation is often a good thing, anyways.

* You should include the optional ``gnome-terminal`` installation
  in the first set of steps, and then the following odd patch is
  also required::

    sudo sed -i 's/<listen>.*<\/listen>/<listen>tcp:host=localhost,port=0<\/listen>/' /etc/dbus-1/session.conf


**Other setup tips (optional, but recommended)**
------------------------------------------------

a. Install Ubuntu terminal fonts as described `HERE
   <https://www.howtogeek.com/249966/how-to-install-and-use-the-linux-bash-shell-on-windows-10/>`_.

#. Follow these ``gnome-terminal`` steps:

   * The default profile "use colors from system theme" shows an
     all-black terminal.  To fix this: select the ``Edit`` tab, then
     ``Profile``, turn **off** "use colors ...", and finally simply
     pick a scheme+palette that you like.

   * In gnome-terminal, everything is standard and similar to other
     linux implementations, and the middle button pastes whatever is
     selected in the BoUoW terminal or other gnome-terminal.
     Shift-Ctrl-c copies, and Shift-Ctrl-v also pastes.

#. Like most Linux systems, some things have to be done with
   ``sudo`` permissions. The username and password may have nothing
   to do with their Windows login. To reset the password, follow
   these instructions:

   * From the default command window, type ``Super[windowskey]+X``,
     then ``A``.  You can change the default user to root::

       lxrun /setdefaultuser root

   * Now BoUoW logs you in as root without asking password. To change
     the user password::

       passwd your_username

   * Change the default user back to your normal user in Windows
     command prompt::

       lxrun /setdefaultuser your_username


.. ---------- HERE/BELOW: copy for all installs --------------

**Make AFNI/SUMA profiles**
---------------------------

.. include:: substep_profiles.rst

**Prepare for Bootcamp (semi-optional)**
----------------------------------------

.. include:: substep_bootcamp.rst


**Evaluate setup/system (important!!)**
---------------------------------------

.. include:: substep_evaluate.rst


**Niceify terminal (optional, but goood)**
------------------------------------------

.. include:: substep_rcfiles.rst


**Keep up-to-date (remember!)**
-------------------------------

.. include:: substep_update.rst




.. figure:: media/AFNI_on_Windows10_2ways.jpg
   :align: center
   :figwidth: 70%
   :name: media/AFNI_on_Windows10_2ways.jpg
   


