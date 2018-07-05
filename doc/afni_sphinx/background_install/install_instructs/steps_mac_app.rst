
.. _install_steps_mac_app:


**macOS**: *The App-based system setup*
========================================

.. contents:: :local:
    :depth: 2

Welcome to the Jungle [#f1]_
---------------------------

The following is a set of scripts packaged as an *app*. It is designed to
automate the AFNI installation steps. You should launch the script by double
clicking the icon like a regular program.

This app will download and install the recommended versions of the following:
 1. Xcode command line tools
 2. Xquartz
 3. R
 4. AFNI

If a package is already installed, you may be asked if you want to reinstall it.
But you can skip it if you like.
This script will also setup your ~/.cshrc, ~/.bashrc, and ~/.bash_profile.
It will add the appropriate entries to your paths specified there.
After completion, you should restart your computer just in case.

This is a **BETA** script, so your feedback is welcome. We have tested the app
on 10.10 to 10.13 with various hardware.

*If you are seeking the non-App version of install instructions,
please see* :ref:`HERE <install_steps_mac>`.

Take the File and Run [#f2]_
---------------------------

* `Download .zip Version <https://afni.nimh.nih.gov/pub/dist/bin/misc/afni_macOS_install_BETA.zip>`_

Break It Down Again [#f3]_
--------------------------

* Download the file and open it to extract or mount.
* Read the README!!
* Double click the "afni_macOS_install_BETA" icon.
* You may get a popup notification about an "unidentified developer".

    .. image:: media/unknown_dev_warning.png
        :width: 40%
        :align: center

    If so, you can continue by:
        * Right click or control click the app icon.
        * Then click open from the shortcut menu.
        * (see here for more info: https://support.apple.com/kb/PH25088)

* This will launch your terminal.
    Most of the rest of the interactions are in the terminal.
    If needed, the Xcode command line tools will popup an installation window.
    Once complete, return to the terminal to continue.

* To accept the default options (shown in [ ]), just press Enter.
    For some sections, you may need to type in your password.
    If you are not an administrator on your computer, this installation may fail.
    Make sure you have permissions for the folder you choose for AFNI!
    The ``sudo`` prompt **will** time out after awhile, so this is
    **not** an unattended install.

* This script does not install fink. Nor does it download the class data.

.. figure:: media/apple_brain_color_flipped.png
    :width: 5%
    :align: center

    *The icon is a work in progress.
    If you have some design skills, please send me alternatives.*

Install Netpbm
--------------

.. include:: substep_netpbm.rst

.. ---------- HERE/BELOW: copy for all installs --------------

Prepare for Bootcamp
--------------------

.. include:: substep_bootcamp.rst

Evaluate setup/system (**important!**)
----------------------------------

.. include:: substep_evaluate.rst

Niceify terminal (optional, but goood)
--------------------------------------

.. include:: substep_rcfiles_mac.rst

Keep up-to-date (remember!)
---------------------------

.. include:: substep_update.rst


.. rubric:: Footnotes

.. [#f1] Guns N' Roses
.. [#f2] Steve Miller Band
.. [#f3] Tears for Fears
