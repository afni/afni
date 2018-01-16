********************
@move.to.series.dirs
********************

.. _@move.to.series.dirs:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    -----------------------------------------------------------------
    @move.to.series.dirs           - partition DICOM files into series directories
    
    Given a set of DICOM files copy or move the files into new series directories.
    Generate a list of series numbers, and for each one, make a directory and copy
    or move the files.
    
    usage: @move.to.series.dirs [options] DICOM_FILES ...
    
    examples:
    
        @move.to.series.dirs -test IMG*
        @move.to.series.dirs -action move IMG*
    
      If the file list is too long for the shell, consider using -glob
      as in the testing example:
    
        @move.to.series.dirs -test -glob 'dir1/IMG*'
    
    terminal option:
    
       -help        : show hist help
       -hist        : show modification history
       -ver         : show version number
    
    processing option:
    
       -action ACTION       : ACTION can be copy or move
                              default = copy
       -dprefix PREFIX      : specify directory root for output series directories
                              default = .
       -tag TAG             : specify tag to use for partitioning
                              default = 0020,0011   (REL Series Number)
       -test                : do not move any file, just show what would be done
    
    ---------------------------------------------
    
    R Reynolds, April, 2013
