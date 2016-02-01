
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
<https://afni.nimh.nih.gov/afni/community/board/>`_.

.. note:: The record of changes (new options, new programs, bug fixes,
          et al.) is maintained for all the see in the online `AFNI
          History
          <https://afni.nimh.nih.gov/pub/dist/doc/misc/history/index.html>`_.

|

Choosing an AFNI that's right for *you*
=======================================

A. Updating an already-installed AFNI (Linux/Unix or Mac)
---------------------------------------------------------

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

B. Downloading a new set of precompiled AFNI binaries (Linux/Unix or Mac)
-------------------------------------------------------------------------

If you don't have AFNI on your computer already, or you want to
manually download a set of binaries, then click on a link below to get
the selected AFNI code for your desired system. For help with
installing various prerequisite tools that AFNI depends on, please see
the :ref:`install_page`.

#. **Recommended binaries for (most) Linux/Unix: Ubuntu, Fedora, etc.**

   \| `OpenMP, 64 bit
   <http://afni.nimh.nih.gov/pub/dist/tgz/linux_openmp_64.tgz>`_ |

   The strongly recommended starting point for most any Linux/Unix
   system.

   If you particularly know that this will not work for you, please
   look in the list of :ref:`Binaries for other systems <other_afni_bin>`, below.

#. **Recommended binaries for (most) Mac OS: 10.7+.**

   \| `Mac OS X Lion (10.7 Intel), 64 bit
   <http://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.7_Intel_64.tgz>`_ |

   The strongly recommended starting point for most modern Mac systems
   (10.7 and higher).

   For Mac OS 10.11 (El Capitan) users, some additional modifications
   to your computer settings are required.  These are currently
   documented `here
   <https://afni.nimh.nih.gov/afni/community/board/read.php?1,149775,149775#msg-149775>`_.

   .. _other_afni_bin:

#. **Binaries for other systems: the rest.**

   * for `Mac OS X Mountain Lion (10.8 Intel), 64 bit
     <http://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.7_Intel_64.tgz>`_.
 
   * for `Mac OS X Snow Leopard (10.6 Intel), 64 bit
     <http://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.6_Intel_64.tgz>`_.

   * for `Mac OS X Snow Leopard (10.6 Intel), 64bit, no fink
     <http://afni.nimh.nih.gov/pub/dist/tgz/macosx_10.6_Intel_64.no.fink.tgz>`_.
   
   * for `Linux xorg7, 64 bit
     <http://afni.nimh.nih.gov/pub/dist/tgz/linux_xorg7_64.tgz>`_.

   * for `Linux xorg7, 32 bit
     <http://afni.nimh.nih.gov/pub/dist/tgz/linux_xorg7.tgz>`_.

   * for `FreeBSD with ports (github)
     <https://github.com/outpaddling/freebsd-ports-wip>`_.

   * for `Solaris 2.9 suncc
     <http://afni.nimh.nih.gov/pub/dist/tgz/solaris29_suncc.tgz>`_.
   
|

C. Downloading AFNI source code
-------------------------------------------------------------------------



|

|

:Date: |today|
