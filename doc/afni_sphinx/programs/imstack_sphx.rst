.. contents:: 
    :depth: 4 

*******
imstack
*******

.. code-block:: none

    Usage: imstack [options] image_filenames ...
    Stacks up a set of 2D images into one big file (a la MGH).
    Options:
      -datum type   Converts the output data file to be 'type',
                      which is either 'short' or 'float'.
                      The default type is the type of the first image.
      -prefix name  Names the output files to be 'name'.b'type' and 'name'.hdr.
