
.. _U_misc_installing_afni:

**************************
Installing AFNI (software)
**************************

To install AFNI, please see `HowTo #0 <http://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/index.html>`_ for descriptions and details.

For help with installation difficulties, please search or post to the `AFNI Message Board <http://afni.nimh.nih.gov/afni/community/board>`_

If you are somewhat confident, consider this quick example for a modern Linux system::

   curl -O http://afni.nimh.nih.gov/pub/dist/tgz/linux_openmp_64.tgz
   tar xfz linux_openmp_64.tgz
   mv linux_openmp_64 ~/abin
   echo 'set path = ( $path $HOME/abin )' >> ~/.cshrc
   source ~/.cshrc

Alternatively, consider this quick example for a general system::

   curl -O http://afni.nimh.nih.gov/pub/dist/bin/linux_openmp_64/@update.afni.binaries
   tcsh @update.afni.binaries -defaults
   echo 'set path = ( $path $HOME/abin )' >> ~/.cshrc
   source ~/.csrhc

To update an existing AFNI installation, consider simply::

   @update.afni.binaries -defaults

After this is done, it may be necessary to open a new terminal window.

.. seealso::

   - `@update.afni.binaries -help <http://afni.nimh.nih.gov/pub/dist/doc/program_help/@update.afni.binaries.html>`_
   - `HowTo #0 - main page <http://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/index.html>`_
   - `HowTo #0 - Linux install <http://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/linux_inst_basic.html>`_
   - `HowTo #0 - Mac install <http://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/mac_10.78.html>`_
   - `AFNI bootcamp class setup <http://afni.nimh.nih.gov/pub/dist/HOWTO/howto/ht00_inst/html/class_setup.html>`_



