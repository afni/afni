***********
nifti1_test
***********

.. _nifti1_test:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: nifti1_test [-n2|-n1|-na|-a2] infile [prefix]
    
     If prefix is given, then the options mean:
      -a2 ==> write an ANALYZE 7.5 file pair: prefix.hdr/prefix.img
      -n2 ==> write a NIFTI-1 file pair: prefix.hdr/prefix.img
      -n1 ==> write a NIFTI-1 single file: prefix.nii
      -na ==> write a NIFTI-1 ASCII+binary file: prefix.nia
      -za2 => write an ANALYZE 7.5 file pair:
              prefix.hdr.gz/prefix.img.gz
      -zn2 => write a NIFTI-1 file pair: prefix.hdr.gz/prefix.img.gz
      -zn1 => write a NIFTI-1 single file: prefix.nii.gz
     The default is '-n1'.
    
     If prefix is not given, then the header info from infile
     file is printed to stdout.
    
     Please note that the '.nia' format is NOT part of the
     NIFTI-1 specification, but is provided mostly for ease
     of visualization (e.g., you can edit a .nia file and
     change some header fields, then rewrite it as .nii)
    
