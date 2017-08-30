
.. _install_overview:


************************
The Indispensable Overview
************************

.. contents::
   :depth: 3

Preamble Ramble
---------------

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

Basically, there's a lotta ins, a lotta outs, a lotta what-have-yous,
a lot of strands to keep in one's head.  Hopefully the present
documentation, available scripts, and helpful `Message Board
polylogues <https://afni.nimh.nih.gov/afni/community/board/>`_
facilitate the installation process. This document is fully intended
to be enhanced and improved over time.  If you would like to help us
by recommending additional sections or details wherever they might be
useful, please post a request to the AFNI `Message Board
<https://afni.nimh.nih.gov/afni/community/board/>`_.

Setting up systems
------------------

Step-by-step instructions for setting up AFNI on a system are provided:

- for :ref:`Linux- Ubuntu <install_steps_linux_ubuntu>`

- for :ref:`Linux- Fedora/Red Hat <install_steps_linux_Fed_RH>`

- for :ref:`Mac OS X <install_steps_mac>`

Methods for checking/evaluating each setup are also described on those
pages.

A page of **Frequently Asked Questions (FAQs)** and suggestions for
commonly-seen error messages also exists :ref:`here
<install_error_msgs>`.

.. _install_tech_notes:

Optional reading: technical notes around installing
---------------------------------------------------

This section of purely extra reading defines/describes some of the
computer system-ese, which may be of use to interested users.
Particularly if one is not used to dealing with these things often,
these notes may give a better sense of the terminology and why things
are the way there are for in the installation process. In describing
where things like default settings and environment variables in AFNI
are, it might also help improve your AFNI experience by pointing out
the ways in which you can customize your settings (simple example-- do
you prefer radiological or neurological views in the viewer?).  But
this is purely optional, and the setup processes can be done
independently.

**Notes:**

.. _tech_notes_PacMan:
- For most Unix/Linux/Mac systems, **package managers** are used to
  facilitate the downloading and installation of most software from
  standard repositories.  Having a central authority performing this
  task on the computer should help to keep it all in line.
  Occasionally, the use of multiple package managers may be required
  on a system, but this is generally avoided.  For example:

  * Fedora has ``yum`` or ``dnf``

  * Ubuntu has ``aptitude`` (so most commands have an ``apt-get`` in
    them)

  * Mac has things like ``Homebrew`` (commands have ``brew`` in them;
    mainly what is used for 10.7+) and ``fink`` (mainly for 10.5-6)

  Each command basically goes through a list of known packages to
  install, along with any dependencies.  Some of installation involves
  making sure that these managers are uptodate, and also pointing them
  to certain repositories for getting very recent versions of
  programs.

  By default, the package manager will ask the user to verify steps
  with yes/no questions, and the ``-y`` option will automatically
  answer "yes" to every prompt, to simplify the user's life.

  .. _tech_notes_Shell:

- AFNI runs mainly in terminal commands, which means that we are
  constantly interacting with a **shell**.  The two main types (or
  *flavors*) of shell are ``tcsh`` and ``bash`` (or ``sh``). The
  choice of shell affects, for instance, the syntax of scripting and
  some of the system setup.  You can see what shell you are using
  with the command::

    which $SHELL

  By default, most AFNI command line instructions assume that ``tcsh``
  is the chosen shell for a terminal.  Also, many Message Board
  postings and scripts in demos, which may be useful for reference,
  are written in ``tcsh``.  So, the choice is yours, but choose
  wisely...

- The choice of shell also determines how the **profile** settings are
  made.  These are necessary for things like telling the shell where
  to look for commands (setting **paths**), for making aliases
  (shortcuts for specific commands) and for other things.  The profile
  files are in the home directory, either ``$HOME/.bashrc`` (for
  ``bash``) or ``$HOME/.cshrc`` (for ``tcsh``), and we will edit these
  while setting up AFNI.

  .. _tech_notes_ENV:

- Many default aspects of how AFNI and SUMA run, what initial viewer
  settings are, how warning messages are displayed, whether templates
  are automatically loaded, etc. are determined with settings of
  **environment variables** that can be changed by the user. These can
  be controlled in files that are automatically checked each time AFNI
  and SUMA run, called ``~/.afnirc`` and ``~/.sumarc``, respectively.
  A large list of malleable environment variables (and their default
  values) are `here for AFNI
  <https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html>`_.

- Many profile and system variables are referenced with a dollar sign
  ``$`` preceding their name, e.g., ``$SHELL``, ``$path``, ``$HOME``
  (which is also represented by the symbol ``~``, and used
  interchangeably below in many situations), etc.  However, note that
  when defining a variable, it doesn't have a ``$`` in its name (but
  it can be defined in terms of variables being referenced with a
  ``$``).

- Some installation features require having root or administrator
  security privileges.  These commands are typically prefaced by the
  word ``sudo``, and when executing (at least the first time), the
  user will be prompted to enter the appropriate password.


|
