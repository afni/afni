******
imstat
******

.. _imstat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Calculation of statistics of one or more images.
    Usage: imstat [-nolabel] [-pixstat prefix] [-quiet] image_file ...
      -nolabel        = don't write labels on each file's summary line
      -quiet          = don't print statistics for each file
      -pixstat prefix = if more than one image file is given, then
                         'prefix.mean' and 'prefix.sdev' will be written
                         as the pixel-wise statistics images of the whole
                         collection.  These images will be in the 'flim'
                         floating point format.  [This option only works
