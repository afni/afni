.. _ahelp_3dPAR2AFNI.pl:

*************
3dPAR2AFNI.pl
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    3dPAR2ANFI
    Version: 2008/07/18 11:12
    
    Command line Options:
    -h     This help message.
    -v     Be verbose in operation.
    -s     Skip the outliers test when converting 4D files
           The default is to perform the outliers test.
    -n     Output NIfTI files instead of HEAD/BRIK.
           The default is create HEAD/BRIK files.
    -a     Output ANALYZE files instead of HEAD/BRIK.
    -o     The name of the directory where the created files should be
           placed.  If this directory does not exist the program exits
           without performing any conversion.
           The default is to place created files in the same directory
           as the PAR files.
    -g     Gzip the files created.
           The default is not to gzip the files.
    -2     2-Byte-swap the files created.
           The default is not to 2 byte-swap.
    -4     4-Byte-swap the files created.
           The default is not to 4 byte-swap.
    
    Sample invocations:
    3dPAR2AFNI subject1.PAR
    	Converts the file subject1.PAR file to subject1+orig.{HEAD,BRIK}
    3dPAR2AFNI -s subject1.PAR
           Same as above but skip the outlier test
    3dPAR2AFNI -n subject1.PAR
           Converts the file subject1.PAR file to subject1.nii
    3dPAR2AFNI -n -s subject1.PAR
           Same as above but skip the outlier test
    3dPAR2AFNI -n -s -o ~/tmp subject1.PAR
           Same as above but skip the outlier test and place the
           created NIfTI files in ~/tmp
    3dPAR2AFNI -n -s -o ~/tmp *.PAR
           Converts all the PAR/REC files in the current directory to
           NIfTI files, skip the outlier test and place the created
