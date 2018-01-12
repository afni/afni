.. contents:: 
    :depth: 4 

**************
fat_roi_row.py
**************

.. code-block:: none

    
    
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ++ Sept, 2014.
    
         ++ Select a single ROI's row out of a connectivity matrix file (*.grid
               or *.netcc) for viewing and/or further analysis.
         ++ written by PA Taylor.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
         TO USE (from a terminal commandline):
    
          $ fat_roi_row.py  -r ROI  { -m MATR_FILES | -l LIST }
         where:
            -r, --roi=ROI            :specify which ROI's row of connectivity you
                                      want to select out.
                                      If you have used labeltables in your tracking
                                      and output, then you may select the ROI by
                                      using either the string label (checked first;
                                      probably should be put in single quotation
                                      marks) or by the ROI mask number.  
            -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                      (*.grid or *.netcc) file(s)- by searchable 
                                      path. This can be a globbable entry in quotes
                                      containing wildcard characters, such as
                                      'DIR1/*/*000.grid'.
            -l, --list_match=LIST    :another way of inputting the matrix
                                      (*.grid or *.netcc) files-- by explicit
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
                                      default naming convention is applied:
                                      NAME.grid   ->  NAME_grid_ROI.row
                                      NAME.netcc  ->  NAME_netcc_ROI.row
                                      where 'ROI' would be the 3-zero-padded 
                                      ROI label.
          -E, --ExternLabsNo         :switch to turn off the writing/usage of 
                                      user-defined labels in the *.grid/*.netcc 
                                      files.  Can't see why this would be desired,
                                      to be honest.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
         Example:
           $ fat_roi_row.py  --roi=3  --matr_in='./GROUP/*/*_000.grid' 
         or, equivalently:
           $ fat_roi_row.py  -r 3  -m './GROUP/*/*_000.grid' 
    
    -----------------------------------------------------------------------------
    
