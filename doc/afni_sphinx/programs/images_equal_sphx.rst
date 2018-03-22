************
images_equal
************

.. _ahelp_images_equal:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: images_equal [-all] fileA fileB
    
    * Simple program to test if 2 2D images are identical.
    * Exit status is 1 if they are equal, and 0 if they are not.
    * If either image cannot be read, then exit status also 0.
    * If the '-all' option is used, then all the images in the files
      are compared, and all must be equal for the exit status to be 1.
    * If '-all' is NOT given, only the first image in each file is
      compared.
    * This program is meant for use in scripts that deal with DICOM
      servers that sometimes deal out multiple copies of the same
      image in different filenames :-(
    * Also see program uniq_images, which works on multiple inputs.
