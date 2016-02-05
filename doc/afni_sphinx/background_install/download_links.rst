
.. _Download_AFNI:

******************************************************
Downloading the latest and greatest AFNI/SUMA software
******************************************************


.. contents::
   :titlesonly:
   :depth: 3
   :maxdepth: 3
   :numbered: 

Overview
========

The direct links for downloading precompiled AFNI binaries, as well as
the compilable source code, are provided here.  Each download includes
all SUMA and FATCAT tools, as well.

As of Jan. 1, 2016, AFNI utilizes a version numbering system, which is
useful for tracking changes, getting help on the `Message Board
<https://afni.nimh.nih.gov/afni/community/board/>`_, reporting in
published work, etc.  The version is reported using a trio of numbers,
corresponding to **"major.minor.micro"** (or roughly,
*"yearly.quarterly.oftenly"*) updates. You can discover the mystery
number on your own system by simply typing::

  afni -ver

This version number should *always* be provided when posting questions on the 
`Message Board
<httpss://afni.nimh.nih.gov/afni/community/board/>`_.

.. note:: The record of changes (new options, new programs, bug fixes,
          et al.) is maintained for all the see in the online `AFNI
          History
          <httpss://afni.nimh.nih.gov/pub/dist/doc/misc/history/index.html>`_.

Some additional introductory reading is available `here
<https://afni.nimh.nih.gov/afni/doc/first>`_.  ***Question: keep
this?***

|


Choosing an AFNI that's right for *you*
=======================================

A. Updating existing AFNI binaries to the newest version (Linux/Unix or Mac)
----------------------------------------------------------------------------

If you have already have a working set of AFNI binaries on your
computer that you are wishing to update, this can be done most simply
by using the following script from the terminal command line::

  @update.afni.binaries -d

This will automatically detect which binary version to download and
where to install it (based on location of the present binaries), and
then it will unpackage the new binaries so you can just keep rollin'
along.  (It will also make an automatic backup of your existing
binaries in a subdirectory called ``auto_backup.BINARY_VERSION/``,
just in case you want it.)

|

B. Downloading a set of the newest precompiled AFNI binaries
------------------------------------------------------------

If you don't have AFNI on your computer already, or if you just want
to download particular a set of binaries, then you can click on a link
below to get the code for your desired system. For help installing the
various prerequisite tools on which AFNI depends (and for seeing handy
command line tools to check if things are OK), please see the
:ref:`install_page`.  After that set-up, then these precompiled codes
should be ready-to-run.

.. note:: Those with only Windows systems have entered a world of
          pain, with respect to using AFNI. The best options are
          likely to procure a computer with a Unix/Linux/Mac operating
          system or to make a dual boot computer (for example, with
          Linux) rather than to install a virtual machine.  It's worth
          it, for a league game.

.. _afni_bin_unix:

- **Recommended binaries for (most) Linux/Unix: Ubuntu, Fedora, etc.**

   \| `OpenMP, 64 bit
   <https://afni.nimh.nih.gov/pub/dist/tgz/linux_openmp_64.tgz>`_ |

   The strongly recommended starting point for most any Linux/Unix
   system.

   If you particularly know that this will not work for you, please
   look in the list of :ref:`Binaries for other systems
   <afni_bin_other>`, below.

   .. _afni_bin_mac:

- **Recommended binaries for (most) Mac OS: 10.7+.**

   \| `Mac OS X Lion (10.7 Intel), 64 bit
   <https://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.7_Intel_64.tgz>`_ |

   The strongly recommended starting point for most modern Mac systems
   (10.7 and higher).

   For Mac OS 10.11 (El Capitan) users, some additional modifications
   to your computer settings are required for smooth sailing.  These
   are currently documented `here
   <httpss://afni.nimh.nih.gov/afni/community/board/read.php?1,149775,149775#msg-149775>`_.

   .. _afni_bin_other:

- **Binaries for other systems: the rest.**

   * for `Mac OS X Mountain Lion (10.8 Intel), 64 bit
     <https://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.7_Intel_64.tgz>`_.
 
   * for `Mac OS X Snow Leopard (10.6 Intel), 64 bit
     <https://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.6_Intel_64.tgz>`_.

   * for `Mac OS X Snow Leopard (10.6 Intel), 64bit, no fink
     <https://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.6_Intel_64.no.fink.tgz>`_.
   
   * for `Linux xorg7, 64 bit
     <https://afni.nimh.nih.gov/pub/dist/tgz/linux_xorg7_64.tgz>`_.

   * for `Linux xorg7, 32 bit
     <https://afni.nimh.nih.gov/pub/dist/tgz/linux_xorg7.tgz>`_.

   * for `Linux gcc32, 32 bit
     <https://afni.nimh.nih.gov/pub/dist/tgz/linux_gcc32.tgzK>`_.

   * for `FreeBSD with ports (github)
     <httpss://github.com/outpaddling/freebsd-ports-wip>`_.

   * for `Solaris 2.9 suncc
     <https://afni.nimh.nih.gov/pub/dist/tgz/solaris29_suncc.tgz>`_.

   * for `Windows Cygwin (not recommended!) 
     <https://afni.nimh.nih.gov/pub/dist/tgz/cygwin.tgz>`_.
   
|

.. _download_SRC:

C. Downloading the newest AFNI source code
------------------------------------------

Another way to get AFNI working on your computer is to compile from
the source itself: 

\| `AFNI Source Code
<https://afni.nimh.nih.gov/pub/dist/tgz/afni_src.tgz>`_ |

There are several usable, example ``Makefile``\s included in the main
``afni_src/`` directory, as well as a couple (mainly for Linux
systems) in ``afni_src/other_builds/``.

In all likelihood this option is pretty much only useful if you are
writing or contributing code yourself, or if your system is
particularly finicky.  Otherwise, it is likely far easier to grab a
set of recommended precompiled binaries of the :ref:`Linux/Unix
<afni_bin_unix>` or :ref:`Mac <afni_bin_mac>` variety (again, sorrry,
Windowers...).

|

D. Browsing all AFNI packages, with bonus files
-----------------------------------------------

The following is a browsable page that contains a tarball for each of
the precompiled platform versions:

`AFNI Software Packages <https://afni.nimh.nih.gov/pub/dist/tgz/>`_

It also contains several standard reference brains and demo data
sets. All files are downloadable by clicking on the links on the above
page, and also by using command line functions such as ``curl`` or
``wget``, such as::
  
  curl -O https://afni.nimh.nih.gov/pub/dist/tgz/TTatlas+tlrc.*
  wget https://afni.nimh.nih.gov/pub/dist/tgz/TTatlas+tlrc.*

NB: for most demo sets, there is an ``@Install_*`` command to procure
and open the directory.

|

|

:Date: |today|
