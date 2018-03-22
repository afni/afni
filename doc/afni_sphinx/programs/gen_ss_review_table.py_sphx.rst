**********************
gen_ss_review_table.py
**********************

.. _ahelp_gen_ss_review_table.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    gen_ss_review_table.py - generate a table from ss_review_basic output files
    
       Given many output text files (e.g. of the form out.ss_review.SUBJECT.txt),
       make a tab-delimited table of output fields, one infile/subject per line.
    
       The program is based on processing lines of the form:
    
            description label : value1 value2 ...
    
       A resulting table will have one row per input, and one column per value,
       with columns separated by a tab character, for input into a spreadsheet.
    
       The top row of the output will have labels.
       The second row will have value_N entries, corresponding to the labels.
       The first column will be either detected group names from the inputs,
          or will simply be the input file names.
    
     * See "gen_ss_review_scripts.py -help_fields" for short descriptions of
       the fields.
    
    ------------------------------------------
    examples:
    
       1. typical usage: input all out.ss_review files across groups and subjects
    
          gen_ss_review_table.py -tablefile review_table.xls        \
                    -infiles group.*/subj.*/*.results/out.ss_review.*
    
       2. just show label table
    
          gen_ss_review_table.py -showlabs -infiles gr*/sub*/*.res*/out.ss_rev*
    
    ------------------------------------------
    terminal options:
    
       -help                : show this help
       -hist                : show the revision history
       -ver                 : show the version number
    
    ------------------------------------------
    process options:
    
       -infiles FILE1 ...   : specify @ss_review_basic output text files to process
    
             e.g. -infiles out.ss_review.subj12345.txt
             e.g. -infiles group.*/subj.*/*.results/out.ss_review.*
    
          The resulting table will be based on all of the fields in these files.
    
          This program can be used as a pipe for input and output, using '-'
          or file stream names.
    
       -overwrite           : overwrite the output -tablefile, if it exists
    
          Without this option, an existing -tablefile will not be overwritten.
    
       -separator SEP       : use SEP for the label/vals separator (default = ':')
    
             e.g. -separator :
             e.g. -separator tab
             e.g. -separator whitespace
    
          Use this option to specify the separation character or string between
          the labels and values.
    
       -showlabs            : display counts of all labels found, with parents
    
          This is mainly to help create a list of labels and parent labels.
    
       -show_missing        : display all missing keys
    
          Show all missing keys from all infiles.
    
       -tablefile OUT_NAME  : write final table to the given file
    
          If the specified file already exists, it will not be overwritten
          unless the -overwrite option is specified.
    
       -verb LEVEL          : be verbose (default LEVEL = 1)
    
    ------------------------------------------
    Thanks to J Jarcho for encouragement and suggestions.
    
    R Reynolds    April 2014
    =============================================================================
