
.. _install_page:

*******************************************
The Authoritative HowTo for Installing AFNI
*******************************************

.. contents::
   :depth: 3

Overview
========

Background
----------

AFNI (with SUMA and FATCAT thrown in for free!) can be used most
easily on Linux/Unix and Mac systems.  Windows users would be advised
to make friends with those who have such systems, or to
purchase/procure their own-- they really aren't that bad anymore.

AFNI is freely available (see the `AFNI README.copyright
<https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.copyright.html>`_),
and makes use of several other freely available software packages and
libraries (such as R, GSL, Python, etc.).  This latter feature is
great, since there is a lot of well-tested and powerful computational
functionality out there, which doesn't have to be written from
scratch.  However, it also means that there are several requirements
for a computer system to be able to run AFNI in full, many of which
might change over time as the other packages develop.

Hopefully the present documentation, available scripts, and helpful
`Message Board polylogues
<https://afni.nimh.nih.gov/afni/community/board/>`_ facilitate the
installation process. This document is fully intended to be enhanced
and improved over time.  If you would like to help us by recommending
additional sections or details wherever they might be useful, please
post a request to the AFNI `Message Board
<https://afni.nimh.nih.gov/afni/community/board/>`_.

Technical notes
---------------

- For most Unix/Linux/Mac systems, *package managers* are used to
  facilitate the downloading and installation of most software from
  standard repositories.  Having a central authority performing this
  task on the computer should help to keep it all in line.
  Occasionally, the use of multiple package managers may be required
  on a system, but this is generally avoided.  For example:

  * Fedora has ``yum``;

  * Ubuntu has ``aptitude`` (so most commands have an ``apt-get`` in
    them);

  * Mac has things like ``Homebrew`` (commands have ``brew`` in them;
    mainly what is used for 10.7+) and ``fink`` (mainly for 10.5-6).

  Some of installation involves making sure that these managers are
  uptodate, and also pointing them to certain repositories for getting
  very recent versions of programs.

- AFNI runs mainly in terminal commands, which means that we are
  constantly interacting with a *shell*.  The two main types (or
  *flavors*) of shell are ``tcsh`` and ``bash`` (or ``sh``). The
  choice of shell affects, for instance, the syntax of scripting and
  some of the system setup.  You can see what shell you are using
  with the command::

    which $shell

  Note that ``bash`` is sensitive to spaces, while ``tcsh`` is not.

- The choice of shell also determines how the *profile* settings are
  made.  These are necessary for things like telling the shell where
  to look for commands (*setting paths*), for making aliases
  (shortcuts for specific commands) and for other things.  The profile
  files are in the home directory, either ``$HOME/.bashrc`` (for
  ``bash``) or ``$HOME/.cshrc`` (for ``tcsh``), and we will edit these
  below while setting up AFNI.

- Many default aspects of how AFNI and SUMA run, what initial viewer
  settings are, how warning messages are displayed, whether templates
  are automatically loaded, etc. are determined with settings of
  *environment variables* that can be changed by the user. These can
  be controlled in files that are automatically checked each time AFNI
  and SUMA run, called ``~/.afnirc`` and ``~/.sumarc``, respectively.
  A large list of malleable environment variables (and their default
  values) are `here for AFNI
  <http://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html>`_.

- Many profile and system variables are referenced with a dollar sign
  ``$`` preceding their name, e.g., ``$shell``, ``$HOME``, ``$path``.
  However, note that when defining a variable, it doesn't have a ``$``
  in its name (but it can be defined in terms of variables being
  referenced with a ``$``).

- Some installation features require having root or administrator
  security privileges.  These commands are typically prefaced by the
  word ``sudo``, and when executing (at least the first time), the
  user will be prompted to enter the appropriate password.

|


Complete system setup for:  **(current) Linux**
===============================================

.. from: https://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_current.html

Here we describe installation and system setup for mainstream Linux
versions that are reasonably modern, such as:

* Fedora 21+
* Ubuntu 14.04+

Several of the following steps are system dependent, for example due
to having different package managers, so we list parallel instructions
for each.

1. Install packages that are needed to run the shell and afni
   programs.  Note that ``tcsh`` might not be on the system yet, so we
   install it also:
        
   * for Fedora 21 (and higher)::
      
      sudo yum install -y tcsh libXp openmotif gsl xorg-x11-fonts-misc   \
                          PyQt4 R-devel netpbm-progs gnome-tweak-tool ed
      sudo yum update -y
      
   * for Ubuntu 14.04 (and higher)::
      
      sudo apt-get install -y tcsh libxp6 xfonts-base python-qt4              \
                              r-base-dev libmotif4 libmotif-dev motif-clients \
                              gsl-bin netpbm gnome-tweak-tool libjpeg62
      sudo apt-get update
      
#. Now that ``tcsh`` is installed, set it as the default shell (if
   desired). Some subsequent instructions assume this shell.  Also,
   many Message Board postings and scripts in demos, which may be
   useful for reference, are written in ``tcsh``.  So, the choice is
   yours, but choose wisely...::

      chsh -s /usr/bin/tcsh

#. Install AFNI.

   Assuming there is nothing yet on the system, the following command
   will create a directory called ``$HOME/abin/`` and install the AFNI
   binaries there.  It will also update the ``$path`` variable in the
   shell profile (e.g., ``$HOME/.cshrc`` for ``tcsh``), so that the
   system knows to look there for commands to execute, and it will set
   up AFNI command tabbing.

   First, get the install script (*this* command actually works for both
   Fedora and Ubuntu systems)::
      
      curl -O https://afni.nimh.nih.gov/pub/dist/bin/linux_fedora_21_64/@update.afni.binaries
      
   Then install the appropriate AFNI package.  Note that most other
   Linux systems will probably work with linux_openmp_64:

   * for Fedora 21 (and higher)::

       tcsh @update.afni.binaries -package linux_fedora_21_64

   * for Ubuntu 14.04 (and higher)::

       tcsh @update.afni.binaries -package linux_openmp_64

#. Reboot.  Consider a 'reboot' at this point.  That would deal with
   system updates, the change in login shell, and an updated path::

      reboot

#. Do a quick test to see that afni works::

      afni -ver

   If this doesn't produce anything constructive immediately, or if
   ``reboot`` was skipped, try starting a new ``tcsh`` shell (e.g., by
   opening a new terminal) and updating the path (again, specifically
   for ``tcsh``)::
      
      tcsh
      set path = ( $path ~/abin )
      rehash
      afni -ver
      
   The final command should show something useful.

   NB: ``@update.afni.binaries`` should have set the path in
   ``$HOME/.cshrc``.  Verify this by visually checking that the same
   'set path' line, above, in the (``tcsh``) profile::

     cat ~/.cshrc

!!!! CONTINUE with STEP 5 from the HOWTO !!!!!!! 

|

Computer system:  **Mac OS**
============================

`More here <https://afni.nimh.nih.gov/afni/doc/howto/0>`_


|

|

:Date: |today|
