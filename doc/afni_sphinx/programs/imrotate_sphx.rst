********
imrotate
********

.. _imrotate:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: imrotate [-linear | -Fourier] dx dy phi input_image output_image
    Shifts and rotates an image:
      dx pixels rightwards (not necessarily an integer)
      dy pixels downwards
      phi degrees clockwise
      -linear means to use bilinear interpolation (default is bicubic)
      -Fourier means to use Fourier interpolaion
