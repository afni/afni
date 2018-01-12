.. contents:: 
    :depth: 4 

****************
@ClustExp_CatLab
****************

.. code-block:: none

    
       ----------------------------------------------------------------------------
       @ClustExp_CatLab - helper script to concatenate and label a group of data sets.
    
          Takes a text file with 2 columns with no header line.
          (there can be more columns, that will be ignored)
          On each row:
    
          The 1st column is the label for each data set e.g. subject ID.
          Labels may be at most 64 characters.
          The same subject ID can be used more than once as in the case of
          a within subject analysis design.
    
          The 2nd column is the data set for that label (with path if needed).
          Columns can be separated by white space or a single comma.
    
          The data sets must be a single subbrik or with a single subbrik selector!
          All data sets must be in the same template space.
    
          Creates an output data set that includes each input data set as a labeled
          subbrik.
          This may be useful for extracting individual level ROI data for a group
          level analysis later with perhaps ClustExp_StatParse.py.
    
       -----------------------------------------------------------------------------
       options:
    
          -prefix PREFIX    : output file name
          -input FILE       : name of file containing the labels and data sets table
          -help             : show this help
    
       -----------------------------------------------------------------------------
       examples:
    
       @ClustExp_CatLab -prefix disco -input subjects.csv
    
       -----------------------------------------------------------------------------
       Justin Rajendra 07/20/2017
