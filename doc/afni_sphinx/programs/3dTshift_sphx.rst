********
3dTshift
********

.. _3dTshift:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dTshift [options] dataset
    Shifts voxel time series from the input dataset so that the separate
    slices are aligned to the same temporal origin.  By default, uses the
    slicewise shifting information in the dataset header (from the 'tpattern'
    input to program to3d).
    
    Method:  detrend -> interpolate -> retrend (optionally)
    
    The input dataset can have a sub-brick selector attached, as documented
    in '3dcalc -help'.
    
    The output dataset time series will be interpolated from the input to
    the new temporal grid.  This may not be the best way to analyze your
    data, but it can be convenient.
    
    Warnings:
    * Please recall the phenomenon of 'aliasing': frequencies above 1/(2*TR) can't
      be properly interpolated.  For most 3D FMRI data, this means that cardiac
      and respiratory effects will not be treated properly by this program.
    
    * The images at the beginning of a high-speed FMRI imaging run are usually
      of a different quality than the later images, due to transient effects
      before the longitudinal magnetization settles into a steady-state value.
      These images should not be included in the interpolation!  For example,
      if you wish to exclude the first 4 images, then the input dataset should
      be specified in the form 'prefix+orig[4..$]'.  Alternatively, you can
      use the '-ignore ii' option.
    
    * It seems to be best to use 3dTshift before using 3dvolreg.
      (But this statement is controversial.)
    
    * If the input dataset does not have any slice timing information, and
      '-tpattern' is not given, then this program just copies the input to
      the output.  [02 Nov 2011 -- formerly, it failed]
    
    Options:
      -verbose      = print lots of messages while program runs
    
      -TR ddd       = use 'ddd' as the TR, rather than the value
                      stored in the dataset header using to3d.
                      You may attach the suffix 's' for seconds,
                      or 'ms' for milliseconds.
    
      -tzero zzz    = align each slice to time offset 'zzz';
                      the value of 'zzz' must be between the
                      minimum and maximum slice temporal offsets.
                N.B.: The default alignment time is the average
                      of the 'tpattern' values (either from the
                      dataset header or from the -tpattern option)
    
      -slice nnn    = align each slice to the time offset of slice
                      number 'nnn' - only one of the -tzero and
                      -slice options can be used.
    
      -prefix ppp   = use 'ppp' for the prefix of the output file;
                      the default is 'tshift'.
    
      -ignore ii    = Ignore the first 'ii' points. (Default is ii=0.)
                      The first ii values will be unchanged in the output
                      (regardless of the -rlt option).  They also will
                      not be used in the detrending or time shifting.
    
      -rlt          = Before shifting, the mean and linear trend
      -rlt+         = of each time series is removed.  The default
                      action is to add these back in after shifting.
                      -rlt  means to leave both of these out of the output
                      -rlt+ means to add only the mean back into the output
                      (cf. '3dTcat -help')
    
      -no_detrend   = Do not remove or restore linear trend.
                      Heptic becomes the default interpolation method.
    
      -Fourier = Use a Fourier method (the default: most accurate; slowest).
      -linear  = Use linear (1st order polynomial) interpolation (least accurate).
      -cubic   = Use the cubic (3rd order) Lagrange polynomial interpolation.
      -quintic = Use the quintic (5th order) Lagrange polynomial interpolation.
      -heptic  = Use the heptic (7th order) Lagrange polynomial interpolation.
    
      -tpattern ttt = use 'ttt' as the slice time pattern, rather
                      than the pattern in the input dataset header;
                      'ttt' can have any of the values that would
                      go in the 'tpattern' input to to3d, described below:
    
       alt+z = altplus   = alternating in the plus direction
       alt+z2            = alternating, starting at slice #1 instead of #0
       alt-z = altminus  = alternating in the minus direction
       alt-z2            = alternating, starting at slice #nz-2 instead of #nz-1
       seq+z = seqplus   = sequential in the plus direction
       seq-z = seqminus  = sequential in the minus direction
       @filename         = read temporal offsets from 'filename'
    
      For example if nz = 5 and TR = 1000, then the inter-slice
      time is taken to be dt = TR/nz = 200.  In this case, the
      slices are offset in time by the following amounts:
    
                 S L I C E   N U M B E R
       tpattern    0   1   2   3   4   Comment
       --------- --- --- --- --- ---   -------------------------------
       altplus     0 600 200 800 400   Alternating in the +z direction
       alt+z2    400   0 600 200 800   Alternating, but starting at #1
       altminus  400 800 200 600   0   Alternating in the -z direction
       alt-z2    800 200 600   0 400   Alternating, starting at #nz-2 
       seqplus     0 200 400 600 800   Sequential  in the +z direction
       seqminus  800 600 400 200   0   Sequential  in the -z direction
    
      If @filename is used for tpattern, then nz ASCII-formatted numbers
      are read from the file.  These indicate the time offsets for each
      slice. For example, if 'filename' contains
         0 600 200 800 400
      then this is equivalent to 'altplus' in the above example.
      (nz = number of slices in the input dataset)
    
    N.B.: if you are using -tpattern, make sure that the units supplied
          match the units of TR in the dataset header, or provide a
          new TR using the -TR option.
    
    As a test of how well 3dTshift interpolates, you can take a dataset
    that was created with '-tpattern alt+z', run 3dTshift on it, and
    then run 3dTshift on the new dataset with '-tpattern alt-z' -- the
    effect will be to reshift the dataset back to the original time
    grid.  Comparing the original dataset to the shifted-then-reshifted
    output will show where 3dTshift does a good job and where it does
    a bad job.
    
    ******* Voxel-Wise Shifting -- New Option [Sep 2011] *******
    
     -voxshift fset = Read in dataset 'fset' and use the values in there
                      to shift each input dataset's voxel's time series a
                      different amount.  The values in 'fset' are NOT in
                      units of time, but rather are fractions of a TR
                      to shift -- a positive value means to shift backwards.
                     * To compute an fset-style dataset that matches the
                       time pattern of an existing dataset, try
           set TR = 2.5
           3dcalc -a 'dset+orig[0..1]' -datum float -prefix Toff -expr "t/${TR}-l"
                       where you first set the shell variable TR to the true TR
                       of the dataset, then create a dataset Toff+orig with the
                       fractional shift of each slice stored in each voxel.  Then
                       the two commands below should give identical outputs:
           3dTshift -ignore 2 -tzero 0 -prefix Dold -heptic dset+orig
           3dTshift -ignore 2 -voxshift Toff+orig -prefix Dnew -heptic dset+orig
    
     Use of '-voxshift' means that options such as '-tzero' and '-tpattern' are
     ignored -- the burden is on you to encode all the shifts into the 'fset'
     dataset somehow.  (3dcalc can be your friend here.)
    
    -- RWCox - 31 October 1999
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
