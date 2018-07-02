
.. _Bootcamping:

***********************
**Basic Bootcamp Prep**
***********************


#. **Get the most uptodate AFNI on your computer.**

   * **IF** you do *not* have AFNI set up on your computer system at
     all, then please see the step-by-step instructions (which
     optionally include installing the Bootcamp data, so then you will
     be all set):

     - for Ubuntu Linux: 

       a. :ref:`versions 15.10 and earlier
          <install_steps_linux_ubuntu>`, or
       #. :ref:`versions 16.04 through 17.10
          <install_steps_linux_ubuntu16>`

     - for other Linux:

       a. :ref:`RedHat, CentOS or Fedora <install_steps_linux_Fed_RH>`

     - for Mac OS X: 
       
       a. :ref:`old-school instructions <install_steps_mac>`, or
       #. :ref:`App-ified version <install_steps_mac_app>`

     - for Windows 10: 
       
       a. :ref:`the modern "Fall Creators Update" version
          <install_steps_windows10>`, or 
       #. :ref:`older (and now out of date)
          "Creators Update" version <install_steps_windows10_beta>`

     - for other Windows users... eek.

     Methods for checking/evaluating each setup are also described on
     those pages.  PLEASE make sure you have verified that all is well
     with AFNI on your computer.

   * **ELSE** (you have AFNI set up on your computer, but you are
     not certain that it is the most up-to-date version), please
     do the following:

     + *if you have installed pre-compiled binaries on your computer (the
       most common approach)*::

         @update.afni.binaries -d

     + *if you compile AFNI from source*, then go :ref:`here
       <download_SRC>` and download the latest source.

   .. _install_bootcamp:

#. **Boot up.**

   .. include:: install_instructs/substep_bootcamp.rst

#. **EVALUATE THE SETUP: an important and useful step in this
   process!**

   .. include:: install_instructs/substep_evaluate.rst
