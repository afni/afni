
.. _U_misc_bg1:

******************
Opening a Terminal
******************

Since the Unix tutorial is all about typing commands at a terminal command prompt, it is of course necessary to be able to open a terminal window.  Terminal icons often look like rounded squares, like small monitor images.

Linux Systems
-------------

   On newer systems, one might get to the terminal by::

         drag mouse to upper left corner ... wait a bit ... click on screen image

   On older systems, there is often a Terminal application under::

         Applications ... System Tools ... Terminal

OS X Terminals
--------------

XQuartz may be the prefered application to start, but new systems do not necesarrily come with it.  In such a case, trying to start :command:`afni` from a (non-XQuartz) terminal will result in the user being prompted to install XQuartz.  That may be the most simple way to install it.

Otherwise (and to find the non-XQuartz Terminal application):

   Use Finder to open the /Applications directory, then enter Utilities, and finally look for XQuartz (or Terminal or X11 on an older system).::

        Applications -> Utilities -> XQuartz
        Applications -> Utilities -> X11
        Applications -> Utilities -> Terminal

   It is usually convenient to drag the desired program to the dock, since (hopefully) it will be used more often than iTunes, say...

   Start X11 (or XQuartz or Terminal), and from the application bar at the top of the screen, choose::

        Applications (from X11 menu bar) -> Terminal

.. note:
   Once X11 (or XQuartz) is installed, the Terminal application will start X11 automatically (10.7+), in case one prefers Terminal to the XQuartz xterm.

