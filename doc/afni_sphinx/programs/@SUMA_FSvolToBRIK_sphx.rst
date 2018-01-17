*****************
@SUMA_FSvolToBRIK
*****************

.. _@SUMA_FSvolToBRIK:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: @SUMA_FSvolToBRIK <FSvoldata> <Prefix>
    A script to convert COR- or .mgz files from FreeSurfer.
     DO NOT use this script for general purpose .mgz conversions
     Use mri_convert instead.
    Example 1: Taking COR- images in mri/orig to BRIK volume
          @SUMA_FSvolToBRIK mri/orig test/cor_afni
    
    Example 2: Taking .mgz volume to BRIK volume
          @SUMA_FSvolToBRIK mri/aseg.mgz test/aseg_afni
    
    To view segmented volumes in AFNI, use the FreeSurfer
    color scale by doing:
       Define Overlay --> Pos? (on)
       Choose continuous (**) colorscale
       Right Click on colorscale --> Choose Colorscale
       Select FreeSurfer_Seg_255
       Set Range to 255
