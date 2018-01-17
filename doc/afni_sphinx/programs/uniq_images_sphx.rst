***********
uniq_images
***********

.. _uniq_images:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: uniq_images fileA fileB ...
    
    * Simple program to read in a list of image filenames,
      determine which files have unique images inside, and
      echo out only a list of the filenames with unique images.
    * This program is meant for use in scripts that deal with DICOM
      servers that sometimes deal out multiple copies of the same
      image in different filenames :-(
