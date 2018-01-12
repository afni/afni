.. contents:: 
    :depth: 4 

*******************
fat_mvm_gridconv.py
*******************

.. code-block:: none

    
    
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ++ Jan, 2015.
         ++ Preprocess some 'old school' (=poorly formatted) *.grid files
               so that they can be fat_mvm_prep'ed for statistical modeling
               using 3dMVM.
         ++ written by PA Taylor.
         
         This program is designed to convert old 3dTrackID output *.grid files
         (which have no labels in '#'-started comments) into modern format.
         This program reads in individual or a set of old *.grid files, and
         outputs new ones in the same folder with '_MOD.grid' postfix (or
         an explicit output prefix can be entered using '--list_match').
         
         This program now also applies to updating *.netcc files that have 
         Pearson correlation ('CC') and Fisher Z-transform matrices ('FZ') but
         no labels in '#'-started comments in the modern format.  The same 
         output naming conventions/options as above apply.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
         TO USE (from a terminal commandline):
    
          $ fat_mvm_gridconv.py { -m MATR_FILES | -l LIST }
         where:
            -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                      (*.grid) files- by searchable path.
                                      This can be a globbable entry in quotes
                                      containing wildcard characters, such as
                                      'DIR1/*/*000.grid'.
            -l, --list_match=LIST    :another way of inputting the matrix
                                      (*.grid) files-- by explicit
                                      path in a text file.
                                      The LIST text file must contain at least
                                      one column:
                                      col 1: path to subject matrix file.
                                      with an optional second column:
                                      col 2: output file names.
                                      (NB: columns must be the same length.)
                                      The first line can be '#'-commented,
                                      which is not read for filenames).
                                      If no second column is given, then the
                                      default '_MOD.grid' postfix is applied.
         
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
         Example:
           $ fat_mvm_gridconv.py --matr_in='./GROUP/*/*_000.grid' 
         or, equivalently:
           $ fat_mvm_gridconv.py -m './GROUP/*/*_000.grid' 
    
    -----------------------------------------------------------------------------
    
