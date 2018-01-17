*************
3dLocalHistog
*************

.. _3dLocalHistog:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dLocalHistog [options] dataset ... 
    
    This program computes, at each voxel, a count of how many times each
    unique value occurs in a neighbhood of that voxel, across all the input
    datasets.
     * The neighborhood is defined by the '-nbhd' option.
     * The input datasets should be in short or byte format, without
       scaling factors attached.
     * You can input float format datasets, but the values will be rounded
       to an integer between -32767 and 32767 before being used.
     * You can also output the overall histogram of the dataset collection,
       via the '-hsave' option (as a 1D file).  This is simply the count of how
       many times each value occurs.
     * For histograms of continuously valued datasets see program 3dLocalstat 
       with option -stat hist* 
    
    OPTIONS
    -------
     -nbhd 'nnn'   = The string 'nnn' defines the region around each
                     voxel that will be extracted for the statistics
                     calculation.  The format of the 'nnn' string is
                     the same as in 3dLocalstat:
                     * 'SPHERE(r)'
                     * 'RECT(a,b,c)'
                     * 'RHDD(a)'
                     * 'TOHD(a)'
                     * If no '-nbhd' option is given, then just the voxel
                       itself is used -- in which case, the input dataset(s)
                       must comprise a total of at least 2 sub-bricks!
    
     -prefix ppp   = Use string 'ppp' as the prefix for the output dataset.
    
     -hsave sss    = Save the overall histogram into file 'sss'.  This file will
                     have 2 columns:   value  count
                     Values with zero count will not be shown in this file.
    
     -lab_file LL  = Use file 'LL' as a label file.  The first column contains
                     the numbers, the second column the corresponding labels.
                     * You can use a column selector to choose the columns you
                       want.  For example, if the first column has the labels
                       and the second the values, use 'filename[1,0]'.
    
     -exclude a..b = Exclude values from 'a' to 'b' from the counting.
                     * Zero (0) will never be excluded.
                     * You can use '-exclude' more than once.
     -excNONLAB    = If '-lab_file' is used, then exclude all values that are NOT
                     in the label file (except for 0, of course).
     -mincount mm  = Exclude values which appear in the overall histogram
                     fewer than 'mm' times.
                     * Excluded  values will be treated as if they are zero
                       (and so appear in the '0:Other' output sub-brick).
                     * The overall histogram output by '-hsave' is NOT altered
                       by the use of '-mincount' or '-exclude' or '-excNONLAB'.
    
     -prob         = Normally, the output dataset is a set of counts.  This
                     option converts each count to a 'probability' by dividing
                     by the total number of counts at each voxel.
                     * The resulting dataset is stored as bytes, in units of
                       0.01, so that p=1 corresponds to 1/0.01=100.
    
     -quiet        = Stop the highly informative progress reports.
    
    OUTPUT DATASET
    --------------
     * For each distinct value a sub-brick is produced.
     * The zero value will be first; after that, the values will appear in
       increasing order.
     * If '-lab_file' is used, then the sub-brick label for a given value's count
       will be of the form 'value:label'; for example, '2013:rh.lingual'.
     * For values NOT in the '-lab_file', the label will just be of the form 'value:'.
     * For the first (value=0) sub-brick, the label will be '0:Other'.
    
    Author: RWCox - April 2013
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
