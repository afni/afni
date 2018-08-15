
.. _install_steps_mac_adminA:


**Mac OS (admin'ed)**: *administrator setup*
============================================

.. contents:: :local:

What to do?
-----------

Here we describe the administrator's part of the AFNI installation and
system setup for *administered* Mac OS versions **10.9+**.  Root
privilege **is** required.

These are accompanied by :ref:`instructions for regular users
<install_steps_mac_adminR>`, which need to be performed after in order
to complete the AFNI setup.


Install Xcode and XQuartz
-------------------------

*  *For OS X >= 10.11,* 

   i. Copy+paste::

        xcode-select --install
        
   #. | Click on "Quick Download" DMG here and install:
      | http://www.xquartz.org 

*  *For OS X 10.9 and 10.10*::

     xcode-select --install
     /Applications/Utilities/X11.app

Install R
---------

.. comment out old
  a. | Click on this link: 
     | https://cran.r-project.org/bin/macosx
     | and then click on the top/latest package to install.

a. | Click here to download a recent (but not the *most*
     recent) version of R:
   | https://cran.r-project.org/bin/macosx/el-capitan/base/R-3.4.1.pkg

#. Install specific R packages for AFNI (specified in the given script)::

      wget https://afni.nimh.nih.gov/pub/dist/bin/misc/@afni_R_package_install
      tcsh @afni_R_package_install -afni


Install Netpbm
--------------

.. include:: ../install_instructs/substep_netpbm.rst
