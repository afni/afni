*******
im2niml
*******

.. _im2niml:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: im2niml imagefile [imagefile ...]
    Converts the input image(s) to a text-based NIML element
    and writes the result to stdout.  Sample usage:
     aiv -p 4444 &
     im2niml zork.jpg | nicat tcp:localhost:4444
