.. contents:: 
    :depth: 4 

******
immask
******

.. code-block:: none

    Usage: immask [-thresh #] [-mask mask_image] [-pos] input_image output_image
    * Masks the input_image and produces the output_image;
    * Use of -thresh # means all pixels with absolute value below # in
       input_image will be set to zero in the output_image
    * Use of -mask mask_image means that only locations that are nonzero
       in the mask_image will be nonzero in the output_image
    * Use of -pos means only positive pixels from input_image will be used
