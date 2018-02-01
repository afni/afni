************
3dSynthesize
************

.. _3dSynthesize:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dSynthesize options
    Reads a '-cbucket' dataset and a '.xmat.1D' matrix from 3dDeconvolve,
    and synthesizes a fit dataset using selected sub-bricks and
    matrix columns.
    
    Options (actually, the first 3 are mandatory)
    ---------------------------------------------
     -cbucket ccc = Read the dataset 'ccc', which should have been
                     output from 3dDeconvolve via the '-cbucket' option.
     -matrix mmm  = Read the matrix 'mmm', which should have been
                     output from 3dDeconvolve via the '-x1D' option.
     -select sss  = Selects specific columns from the matrix (and the
                     corresponding coefficient sub-bricks from the
                     cbucket).  The string 'sss' can be of the forms:
                       baseline  = All baseline coefficients.
                       polort    = All polynomial baseline coefficients
                                   (skipping -stim_base coefficients).
                       allfunc   = All coefficients that are NOT marked
                                   (in the -matrix file) as being in
                                   the baseline (i.e., all -stim_xxx
                                   values except those with -stim_base)
                       allstim   = All -stim_xxx coefficients, including
                                   those with -stim_base.
                       all       = All coefficients (should give results
                                   equivalent to '3dDeconvolve -fitts').
                       something = All columns/coefficients that match
                                   this -stim_label from 3dDeconvolve
                                   [to be precise, all columns whose   ]
                                   [-stim_label starts with 'something']
                                   [will be selected for inclusion.    ]
                       digits    = Columns can also be selected by
                                   numbers (starting at 0), or number
                                   ranges of the form 3..7 and 3-7.
                                   [A string is a number range if it]
                                   [comprises only digits and the   ]
                                   [characters '.' and/or '-'.      ]
                                   [Otherwise, it is used to match  ]
                                   [a -stim_label.                  ]
                     More than one '-select sss' option can be used, or
                     you can put more than one string after the '-select',
                     as in this example:
                       3dSynthesize -matrix fred.xmat.1D -cbucket fred+orig \
                                    -select baseline FaceStim -prefix FS
                     which synthesizes the baseline and 'FaceStim'
                     responses together, ignoring any other stimuli
                     in the dataset and matrix.
     -dry         = Don't compute the output, just check the inputs.
     -TR dt       = Set TR in the output to 'dt'.  The default value
                     of TR is read from the header of the matrix file.
     -prefix ppp  = Output result into dataset with name 'ppp'.
    
     -cenfill xxx = Determines how censored time points from the
                     3dDeconvolve run will be filled.  'xxx' is one of:
                       zero    = 0s will be put in at all censored times
                       nbhr    = average of non-censored neighboring times
                       none    = don't put the censored times in at all
                                 (in which  case the created  dataset is)
                                 (shorter than the input to 3dDeconvolve)
                     If you don't give some -cenfill option, the default
                     operation is 'zero'.  This default is different than
                     previous versions of this program, which did 'none'.
              **N.B.: You might like the program to compute the model fit
                      at the censored times, like it does at all others.
                      This CAN be done if you input the matrix file saved
                      by the '-x1D_uncensored' option in 3dDeconvolve.
    
    NOTES:
    -- You could do the same thing in 3dcalc, but this way is simpler
       and faster.  But less flexible, of course.
    -- The output dataset is always stored as floats.
    -- The -cbucket dataset must have the same number of sub-bricks as
       the input matrix has columns.
    -- Each column in the matrix file is a time series, used to model
       some component of the data time series at each voxel.
    -- The sub-bricks of the -cbucket dataset give the weighting
       coefficients for these model time series, at each voxel.
    -- If you want to calculate a time series dataset wherein the original
       time series data has the baseline subtracted, then you could
       use 3dSynthesize to compute the baseline time series dataset, and
       then use 3dcalc to subtract that dataset from the original dataset.
    -- Other similar applications are left to your imagination.
    -- To see the column labels stored in matrix file 'fred.xmat.1D', type
       the Unix command 'grep ColumnLabels fred.xmat.1D'; sample output:
     # ColumnLabels = "Run#1Pol#0 ; Run#1Pol#1 ; Run#2Pol#0 ; Run#2Pol#1 ;
                       FaceStim#0 ; FaceStim#1 ; HouseStim#0 ; HouseStim#1"
       which shows the 4 '-polort 1' baseline parameters from 2 separate
       imaging runs, and then 2 parameters each for 'FaceStim' and
       'HouseStim'.
    -- The matrix file written by 3dDeconvolve has an XML-ish header
       before the columns of numbers, stored in '#' comment lines.
       If you want to generate your own 'raw' matrix file, without this
       header, you can still use 3dSynthesize, but then you can only use
       numeric '-select' options (or 'all').
    -- When using a 'raw' matrix, you'll probably also want the '-TR' option.
    -- When putting more than one string after '-select', do NOT combine
       these separate strings togther in quotes.  If you do, they will be
       seen as a single string, which almost surely won't match anything.
    -- Author: RWCox -- March 2007
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
