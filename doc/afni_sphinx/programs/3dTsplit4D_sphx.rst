**********
3dTsplit4D
**********

.. _ahelp_3dTsplit4D:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    USAGE: 3dTsplit4D [options] dataset
    
    This program converts a 3D+time dataset into multiple 3D single-brick
    files.  The main purpose of this is to accelerate the process of
    export AFNI/NIFTI datasets if you have the unfortunate need to work
    with Some other PrograM that doesn't like datasets in the pseudo-4D
    nature that AFNI knows and loves.
    
    examples:
    
       1. Write the 152 time point dataset, epi_r1+orig, to 152 single
          volume datasets, out/epi.000+orig ... epi.151+orig.
    
             3dTsplit4D -prefix out/epi epi_r1+orig
    
       2. Do the same thing, but write to 152 NIFTI volume datasets,
          out/epi.000.nii ... out/epi.151.nii.  Include .nii in -prefix.
    
             3dTsplit4D -prefix out/epi.nii epi_r1+orig
    
     -prefix PREFIX : Prefix of the output datasets
                      Numbers will be added after the prefix to denote
                      prior sub-brick.
     -keep_datum    : output uses original datum (no conversion to float)
     -digits DIGITS : number of digits to use for output filenames
    
    
    Authored by: Peter Molfese, UConn
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
