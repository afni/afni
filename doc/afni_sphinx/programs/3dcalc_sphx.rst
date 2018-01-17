******
3dcalc
******

.. _3dcalc:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Program: 3dcalc                                                         
    Author:  RW Cox et al                                                   
                                                                            
    3dcalc - AFNI's calculator program                                      
                                                                            
         This program does voxel-by-voxel arithmetic on 3D datasets         
         (only limited inter-voxel computations are possible).              
                                                                            
         The program assumes that the voxel-by-voxel computations are being 
         performed on datasets that occupy the same space and have the same 
         orientations.                                                      
                                                                            
         3dcalc has a lot of input options, as its capabilities have grown  
         over the years.  So this 'help' output has gotten kind of long.    
                                                                            
         For simple voxel-wise averaging of datasets:    cf. 3dMean         
         For averaging along the time axis:              cf. 3dTstat        
         For smoothing in time:                          cf. 3dTsmooth      
         For statistics from a region around each voxel: cf. 3dLocalstat    
                                                                            
    ------------------------------------------------------------------------
    Usage:                                                                  
    -----                                                                   
           3dcalc -a dsetA [-b dsetB...] \                                 
                  -expr EXPRESSION       \                                 
                  [options]                                                 
                                                                            
    Examples:                                                               
    --------                                                                
    1. Average datasets together, on a voxel-by-voxel basis:                
                                                                            
         3dcalc -a fred+tlrc -b ethel+tlrc -c lucy+tlrc \
                -expr '(a+b+c)/3' -prefix subjects_mean                     
                                                                            
       Averaging datasets can also be done by programs 3dMean and 3dmerge.  
       Use 3dTstat to averaging across sub-bricks in a single dataset.      
                                                                            
    2. Perform arithmetic calculations between the sub-bricks of a single   
       dataset by noting the sub-brick number on the command line:          
                                                                            
         3dcalc -a 'func+orig[2]' -b 'func+orig[4]' -expr 'sqrt(a*b)'       
                                                                            
    3. Create a simple mask that consists only of values in sub-brick #0    
       that are greater than 3.14159:                                       
                                                                            
         3dcalc -a 'func+orig[0]' -expr 'ispositive(a-3.14159)' \
                -prefix mask                                                
                                                                            
    4. Normalize subjects' time series datasets to percent change values in 
       preparation for group analysis:                                      
                                                                            
       Voxel-by-voxel, the example below divides each intensity value in    
       the time series (epi_r1+orig) with the voxel's mean value (mean+orig)
       to get a percent change value. The 'ispositive' command will ignore  
       voxels with mean values less than 167 (i.e., they are labeled as     
      'zero' in the output file 'percent_change+orig') and are most likely  
       background/noncortical voxels.                                       
                                                                            
         3dcalc -a epi_run1+orig -b mean+orig     \
                -expr '100 * a/b * ispositive(b-167)' -prefix percent_chng  
                                                                            
    5. Create a compound mask from a statistical dataset, where 3 stimuli   
       show activation.                                                     
          NOTE: 'step' and 'ispositive' are identical expressions that can  
                be used interchangeably:                                    
                                                                            
         3dcalc -a 'func+orig[12]' -b 'func+orig[15]' -c 'func+orig[18]' \
                -expr 'step(a-4.2)*step(b-2.9)*step(c-3.1)'              \
                -prefix compound_mask                                       
                                                                            
       In this example, all 3 statistical criteria must be met at once for  
       a voxel to be selected (value of 1) in this mask.                    
                                                                            
    6. Same as example #5, but this time create a mask of 8 different values
       showing all combinations of activations (i.e., not only where        
       everything is active, but also each stimulus individually, and all   
       combinations).  The output mask dataset labels voxel values as such: 
                                                                            
            0 = none active    1 = A only active    2 = B only active       
            3 = A and B only   4 = C only active    5 = A and C only        
            6 = B and C only   7 = all A, B, and C active                   
                                                                            
         3dcalc -a 'func+orig[12]' -b 'func+orig[15]' -c 'func+orig[18]' \
                -expr 'step(a-4.2)+2*step(b-2.9)+4*step(c-3.1)'          \
                -prefix mask_8                                              
                                                                            
       In displaying such a binary-encoded mask in AFNI, you would probably 
       set the color display to have 8 discrete levels (the '#' menu).      
                                                                            
    7. Create a region-of-interest mask comprised of a 3-dimensional sphere.
       Values within the ROI sphere will be labeled as '1' while values     
       outside the mask will be labeled as '0'. Statistical analyses can    
       then be done on the voxels within the ROI sphere.                    
                                                                            
       The example below puts a solid ball (sphere) of radius 3=sqrt(9)     
       about the point with coordinates (x,y,z)=(20,30,70):                 
                                                                            
         3dcalc -a anat+tlrc                                              \
                -expr 'step(9-(x-20)*(x-20)-(y-30)*(y-30)-(z-70)*(z-70))' \
                -prefix ball                                                
                                                                            
       The spatial meaning of (x,y,z) is discussed in the 'COORDINATES'     
       section of this help listing (far below).                            
                                                                            
    8. Some datsets are 'short' (16 bit) integers with a scalar attached,   
       which allow them to be smaller than float datasets and to contain    
       fractional values.                                                   
                                                                            
       Dataset 'a' is always used as a template for the output dataset. For 
       the examples below, assume that datasets d1+orig and d2+orig consist 
       of small integers.                                                   
                                                                            
       a) When dividing 'a' by 'b', the result should be scaled, so that a  
          value of 2.4 is not truncated to '2'. To avoid this truncation,   
          force scaling with the -fscale option:                            
                                                                            
            3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -fscale   
                                                                            
       b) If it is preferable that the result is of type 'float', then set  
          the output data type (datum) to float:                            
                                                                            
            3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot \
                    -datum float                                            
                                                                            
       c) Perhaps an integral division is desired, so that 9/4=2, not 2.24. 
          Force the results not to be scaled (opposite of example 8a) using 
          the -nscale option:                                               
                                                                            
            3dcalc -a d1+orig -b d2+orig -expr 'a/b' -prefix quot -nscale   
                                                                            
    9. Compare the left and right amygdala between the Talairach atlas,     
       and the CA_N27_ML atlas.  The result will be 1 if TT only, 2 if CA   
       only, and 3 where they overlap.                                      
                                                                            
         3dcalc -a 'TT_Daemon::amygdala' -b 'CA_N27_ML::amygdala' \
                -expr 'step(a)+2*step(b)'  -prefix compare.maps             
                                                                            
       (see 'whereami -help' for more information on atlases)               
                                                                            
    10. Convert a dataset from AFNI short format storage to NIfTI-1 floating
        point (perhaps for input to an non-AFNI program that requires this):
                                                                            
          3dcalc -a zork+orig -prefix zfloat.nii -datum float -expr 'a'     
                                                                            
        This operation could also be performed with program 3dAFNItoNIFTI.  
                                                                            
    11. Compute the edge voxels of a mask dataset.  An edge voxel is one    
        that shares some face with a non-masked voxel.  This computation    
        assumes 'a' is a binary mask (particularly for 'amongst').          
                                                                            
          3dcalc -a mask+orig -prefix edge                     \
                 -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k     \
                 -expr 'a*amongst(0,b,c,d,e,f,g)'                           
                                                                            
        consider similar erode or dilate operations:                        
            erosion:  -expr 'a*(1-amongst(0,b,c,d,e,f,g))'                  
            dilation: -expr 'amongst(1,a,b,c,d,e,f,g)'                      
                                                                            
    ------------------------------------------------------------------------
    ARGUMENTS for 3dcalc (must be included on command line):                
    ---------                                                               
                                                                            
     -a dname    = Read dataset 'dname' and call the voxel values 'a' in the
                   expression (-expr) that is input below. Up to 26 dnames  
                   (-a, -b, -c, ... -z) can be included in a single 3dcalc  
                   calculation/expression.                                  
                   ** If some letter name is used in the expression, but    
                      not present in one of the dataset options here, then  
                      that variable is set to 0.                            
                   ** You can use the subscript '[]' method                 
                      to select sub-bricks of datasets, as in               
                         -b dname+orig'[3]'                                 
                   ** If you just want to test some 3dcalc expression,      
                      you can supply a dataset 'name' of the form           
                        jRandomDataset:64,64,16,40                          
                      to have the program create and use a dataset          
                      with a 3D 64x64x16 grid, with 40 time points,         
                      filled with random numbers (uniform on [-1,1]).       
                                                                            
     -expr       = Apply the expression - within quotes - to the input      
                   datasets (dnames), one voxel at time, to produce the     
                   output dataset.                                          
                   ** You must use 1 and only 1 '-expr' option!             
                                                                            
     NOTE: If you want to average or sum up a lot of datasets, programs     
           3dTstat and/or 3dMean and/or 3dmerge are better suited for these 
           purposes.  A common request is to increase the number of input   
           datasets beyond 26, but in almost all cases such users simply    
           want to do simple addition!                                      
                                                                            
     NOTE: If you want to include shell variables in the expression (or in  
           the dataset sub-brick selection), then you should use double     
           "quotes" and the '$' notation for the shell variables; this    
           example uses csh notation to set the shell variable 'z':         
                                                                            
             set z = 3.5                                                    
             3dcalc -a moose.nii -prefix goose.nii -expr "a*$z"           
                                                                            
           The shell will not expand variables inside single 'quotes',      
           and 3dcalc's parser will not understand the '$' character.       
                                                                            
     NOTE: You can use the ccalc program to play with the expression        
           evaluator, in order to get a feel for how it works and           
           what it accepts.                                                 
                                                                            
    ------------------------------------------------------------------------
     OPTIONS for 3dcalc:                                                    
     -------                                                                
                                                                            
      -help      = Show this help.
                                                                            
      -verbose   = Makes the program print out various information as it    
                   progresses.                                              
                                                                            
      -datum type= Coerce the output data to be stored as the given type,   
                   which may be byte, short, or float.                      
                   [default = datum of first input dataset]                 
      -float }                                                              
      -short }   = Alternative options to specify output data format.       
      -byte  }                                                              
                                                                            
      -fscale    = Force scaling of the output to the maximum integer       
                   range. This only has effect if the output datum is byte  
                   or short (either forced or defaulted). This option is    
                   often necessary to eliminate unpleasant truncation       
                   artifacts.                                               
                     [The default is to scale only if the computed values   
                      seem to need it -- are all <= 1.0 or there is at      
                      least one value beyond the integer upper limit.]      
                                                                            
                    ** In earlier versions of 3dcalc, scaling (if used) was 
                       applied to all sub-bricks equally -- a common scale  
                       factor was used.  This would cause trouble if the    
                       values in different sub-bricks were in vastly        
                       different scales. In this version, each sub-brick    
                       gets its own scale factor. To override this behavior,
                       use the '-gscale' option.                            
                                                                            
      -gscale    = Same as '-fscale', but also forces each output sub-brick 
                   to get the same scaling factor.  This may be desirable   
                   for 3D+time datasets, for example.                       
                ** N.B.: -usetemp and -gscale are incompatible!!            
                                                                            
      -nscale    = Don't do any scaling on output to byte or short datasets.
                   This may be especially useful when operating on mask     
                   datasets whose output values are only 0's and 1's.       
                      ** Only use this option if you are sure you           
                         want the output dataset to be integer-valued!      
                                                                            
      -prefix pname = Use 'pname' for the output dataset prefix name.       
                      [default='calc']                                      
                                                                            
      -session dir  = Use 'dir' for the output dataset session directory.   
                      [default='./'=current working directory]              
                      You can also include the output directory in the      
                      'pname' parameter to the -prefix option.              
                                                                            
      -usetemp      = With this option, a temporary file will be created to 
                      hold intermediate results.  This will make the program
                      run slower, but can be useful when creating huge      
                      datasets that won't all fit in memory at once.        
                    * The program prints out the name of the temporary      
                      file; if 3dcalc crashes, you might have to delete     
                      this file manually.                                   
                   ** N.B.: -usetemp and -gscale are incompatible!!         
                                                                            
      -dt tstep     = Use 'tstep' as the TR for "manufactured" 3D+time    
        *OR*          datasets.                                             
      -TR tstep     = If not given, defaults to 1 second.                   
                                                                            
      -taxis N      = If only 3D datasets are input (no 3D+time or .1D files),
        *OR*          then normally only a 3D dataset is calculated.  With  
      -taxis N:tstep: this option, you can force the creation of a time axis
                      of length 'N', optionally using time step 'tstep'.  In
                      such a case, you will probably want to use the pre-   
                      defined time variables 't' and/or 'k' in your         
                      expression, or each resulting sub-brick will be       
                      identical. For example:                               
                      '-taxis 121:0.1' will produce 121 points in time,     
                      spaced with TR 0.1.                                   
                                                                            
                N.B.: You can also specify the TR using the -dt option.     
                N.B.: You can specify 1D input datasets using the           
                      '1D:n@val,n@val' notation to get a similar effect.    
                      For example:                                          
                         -dt 0.1 -w '1D:121@0'                              
                      will have pretty much the same effect as              
                         -taxis 121:0.1
                N.B.: For both '-dt' and '-taxis', the 'tstep' value is in 
                      seconds.                                             
                                                                            
      -rgbfac A B C = For RGB input datasets, the 3 channels (r,g,b) are    
                      collapsed to one for the purposes of 3dcalc, using the
                      formula value = A*r + B*g + C*b                       
                                                                            
                      The default values are A=0.299 B=0.587 C=0.114, which 
                      gives the grayscale intensity.  To pick out the Green 
                      channel only, use '-rgbfac 0 1 0', for example.  Note 
                      that each channel in an RGB dataset is a byte in the  
                      range 0..255.  Thus, '-rgbfac 0.001173 0.002302 0.000447'
                      will compute the intensity rescaled to the range 0..1.0
                      (i.e., 0.001173=0.299/255, etc.)                      
                                                                            
      -cx2r METHOD  = For complex input datasets, the 2 channels must be    
                      converted to 1 real number for calculation.  The      
                      methods available are:  REAL  IMAG  ABS  PHASE        
                    * The default method is ABS = sqrt(REAL^2+IMAG^2)       
                    * PHASE = atan2(IMAG,REAL)                              
                    * Multiple '-cx2r' options can be given:                
                        when a complex dataset is given on the command line,
                        the most recent previous method will govern.        
                        This also means that for -cx2r to affect a variable 
                        it must precede it. For example, to compute the     
                        phase of data in 'a' you should use                 
                     3dcalc -cx2r PHASE -a dft.lh.TS.niml.dset -expr 'a'    
                        However, the -cx2r option will have no effect in    
                     3dcalc -a dft.lh.TS.niml.dset -cx2r PHASE -expr 'a'    
                        which will produce the default ABS of 'a'           
                        The -cx2r option in the latter example only applies 
                        to variables that will be defined after it.         
                        When in doubt, check your output.                   
                    * If a complex dataset is used in a differential        
                        subscript, then the most recent previous -cx2r      
                        method applies to the extraction; for example       
                          -cx2r REAL -a cx+orig -cx2r IMAG -b 'a[0,0,0,0]'  
                        means that variable 'a' refers to the real part     
                        of the input dataset and variable 'b' to the        
                        imaginary part of the input dataset.                
                    * 3dcalc cannot be used to CREATE a complex dataset!    
                        [See program 3dTwotoComplex for that purpose.]      
                                                                            
      -sort         = Sort each output brick separately, before output:     
      -SORT           'sort' ==> increasing order, 'SORT' ==> decreasing.   
                      [This is useful only under unusual circumstances!]    
                      [Sorting is done in spatial indexes, not in time.]    
                      [Program 3dTsort will sort voxels along time axis]    
                                                                            
    ------------------------------------------------------------------------
    DATASET TYPES:                                                          
    -------------                                                           
                                                                            
     The most common AFNI dataset types are 'byte', 'short', and 'float'.   
                                                                            
     A byte value is an 8-bit signed integer (0..255), a short value ia a   
     16-bit signed integer (-32768..32767), and a float value is a 32-bit   
     real number.  A byte value has almost 3 decimals of accuracy, a short  
     has almost 5, and a float has approximately 7 (from a 23+1 bit         
     mantissa).                                                             
                                                                            
     Datasets can also have a scalar attached to each sub-brick. The main   
     use of this is allowing a short type dataset to take on non-integral   
     values, while being half the size of a float dataset.                  
                                                                            
     As an example, consider a short dataset with a scalar of 0.0001. This  
     could represent values between -32.768 and +32.767, at a resolution of 
     0.001.  One could represnt the difference between 4.916 and 4.917, for 
     instance, but not 4.9165. Each number has 15 bits of accuracy, plus a  
     sign bit, which gives 4-5 decimal places of accuracy. If this is not   
     enough, then it makes sense to use the larger type, float.             
                                                                            
    ------------------------------------------------------------------------
    3D+TIME DATASETS:                                                       
    ----------------                                                        
                                                                            
     This version of 3dcalc can operate on 3D+time datasets.  Each input    
     dataset will be in one of these conditions:                            
                                                                            
       (A) Is a regular 3D (no time) dataset; or                            
       (B) Is a 3D+time dataset with a sub-brick index specified ('[3]'); or
       (C) Is a 3D+time dataset with no sub-brick index specified ('-b').   
                                                                            
     If there is at least one case (C) dataset, then the output dataset will
     also be 3D+time; otherwise it will be a 3D dataset with one sub-brick. 
     When producing a 3D+time dataset, datasets in case (A) or (B) will be  
     treated as if the particular brick being used has the same value at each
     point in time.                                                         
                                                                            
     Multi-brick 'bucket' datasets may also be used.  Note that if multi-brick
     (bucket or 3D+time) datasets are used, the lowest letter dataset will  
     serve as the template for the output; that is, '-b fred+tlrc' takes    
     precedence over '-c wilma+tlrc'.  (The program 3drefit can be used to  
     alter the .HEAD parameters of the output dataset, if desired.)         
                                                                            
    ------------------------------------------------------------------------
    INPUT DATASET NAMES
    -------------------
     An input dataset is specified using one of these forms:
        'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.
     You can also add a sub-brick selection list after the end of the
     dataset name.  This allows only a subset of the sub-bricks to be
     read in (by default, all of a dataset's sub-bricks are input).
     A sub-brick selection list looks like one of the following forms:
       fred+orig[5]                     ==> use only sub-brick #5
       fred+orig[5,9,17]                ==> use #5, #9, and #17
       fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8
       fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13
     Sub-brick indexes start at 0.  You can use the character '$'
     to indicate the last sub-brick in a dataset; for example, you
     can select every third sub-brick by using the selection list
       fred+orig[0..$(3)]
    
     N.B.: The sub-bricks are read in the order specified, which may
     not be the order in the original dataset.  For example, using
       fred+orig[0..$(2),1..$(2)]
     will cause the sub-bricks in fred+orig to be input into memory
     in an interleaved fashion.  Using
       fred+orig[$..0]
     will reverse the order of the sub-bricks.
    
     N.B.: You may also use the syntax <a..b> after the name of an input 
     dataset to restrict the range of values read in to the numerical
     values in a..b, inclusive.  For example,
        fred+orig[5..7]<100..200>
     creates a 3 sub-brick dataset with values less than 100 or
     greater than 200 from the original set to zero.
     If you use the <> sub-range selection without the [] sub-brick
     selection, it is the same as if you had put [0..$] in front of
     the sub-range selection.
    
     N.B.: Datasets using sub-brick/sub-range selectors are treated as:
      - 3D+time if the dataset is 3D+time and more than 1 brick is chosen
      - otherwise, as bucket datasets (-abuc or -fbuc)
        (in particular, fico, fitt, etc datasets are converted to fbuc!)
    
     N.B.: The characters '$ ( ) [ ] < >'  are special to the shell,
     so you will have to escape them.  This is most easily done by
     putting the entire dataset plus selection list inside forward
     single quotes, as in 'fred+orig[5..7,9]', or double quotes "x".
                                                                            
    CATENATED AND WILDCARD DATASET NAMES
    ------------------------------------
     Datasets may also be catenated or combined in memory, as if one first
     ran 3dTcat or 3dbucket.
     
     An input with space-separated elements will be read as a concatenated
     dataset, as with 'dset1+tlrc dset2+tlrc dset3+tlrc', or with paths,
     'dir/dset1+tlrc dir/dset2+tlrc dir/dset3+tlrc'.
     The datasets will be combined (as if by 3dTcat) and then treated as a
     single input dataset.  Note that the quotes are required to specify
     them as a single argument.
     
     Sub-brick selection using '[]' works with space separated dataset
     names.  If the selector is at the end, it is considered global and
     applies to all inputs.  Otherwise, it applies to the adjacent input.
     For example:
        local:  'dset1+tlrc[2,3] dset2+tlrc[7,0,1] dset3+tlrc[5,0,$]'
        global: 'dset1+tlrc dset2+tlrc dset3+tlrc[5,6]'
     
     N.B. If AFNI_PATH_SPACES_OK is set to Yes, will be considered as part
     of the dataset name, and not as a separator between them.
     
     Similar treatment applies when specifying datasets using a wildcard
     pattern, using '*' or '?', as in: 'dset*+tlrc.HEAD'.  Any sub-brick
     selectors would apply to all matching datasets, as with:
        'dset*+tlrc.HEAD[2,5,3]'
     
     N.B.: complete filenames are required when using wildcard matching,
     or no files will exist to match, e.g. 'dset*+tlrc' would not work.
     
     N.B.: '[]' are processed as sub-brick or time point selectors.  They
     are therefore not allowed as wildcard characters in this context.
     
     Space and wildcard catenation can be put together.  In such a case,
     spaces divide the input into wildcard pieces, which are processed
     individually.
     
     Examples (each is processed as a single, combined dataset):
     
        'dset1+tlrc dset2+tlrc dset3+tlrc'
        'dset1+tlrc dset2+tlrc dset3+tlrc[2,5,3]'
        'dset1+tlrc[3] dset2+tlrc[0,1] dset3+tlrc[3,0,1]'
     
        'dset*+tlrc.HEAD'
        'dset*+tlrc.HEAD[2,5,3]'
        'dset1*+tlrc.HEAD[0,1] dset2*+tlrc.HEAD[7,8]'
     
        'group.*/subj.*/stats*+tlrc.HEAD[7]'
                                                                            
    ------------------------------------------------------------------------
    1D TIME SERIES:                                                         
    --------------                                                          
                                                                            
     You can also input a '*.1D' time series file in place of a dataset.    
     In this case, the value at each spatial voxel at time index n will be  
     the same, and will be the n-th value from the time series file.        
     At least one true dataset must be input.  If all the input datasets    
     are 3D (single sub-brick) or are single sub-bricks from multi-brick    
     datasets, then the output will be a 'manufactured' 3D+time dataset.    
                                                                            
     For example, suppose that 'a3D+orig' is a 3D dataset:                  
                                                                            
       3dcalc -a a3D+orig -b b.1D -expr "a*b"                             
                                                                            
     The output dataset will 3D+time with the value at (x,y,z,t) being      
     computed by a3D(x,y,z)*b(t).  The TR for this dataset will be set      
     to 'tstep' seconds -- this could be altered later with program 3drefit.
     Another method to set up the correct timing would be to input an       
     unused 3D+time dataset -- 3dcalc will then copy that dataset's time    
     information, but simply do not use that dataset's letter in -expr.     
                                                                            
     If the *.1D file has multiple columns, only the first read will be     
     used in this program.  You can select a column to be the first by      
     using a sub-vector selection of the form 'b.1D[3]', which will         
     choose the 4th column (since counting starts at 0).                    
                                                                            
     '{...}' row selectors can also be used - see the output of '1dcat -help'
     for more details on these.  Note that if multiple timeseries or 3D+time
     or 3D bucket datasets are input, they must all have the same number of 
     points along the 'time' dimension.                                     
                                                                            
     N.B.: To perform calculations ONLY on .1D files, use program 1deval.   
           3dcalc takes .1D files for use in combination with 3D datasets!  
                                                                            
     N.B.: If you auto-transpose a .1D file on the command line, (by ending 
           the filename with \'), then 3dcalc will NOT treat it as the     
           special case described above, but instead will treat it as       
           a normal dataset, where each row in the transposed input is a    
           'voxel' time series.  This would allow you to do differential    
           subscripts on 1D time series, which program 1deval does not      
           implement.  For example:                                         
                                                                            
            3dcalc -a '1D: 3 4 5 6'\' -b a+l -expr 'sqrt(a+b)' -prefix -   
                                                                            
           This technique allows expression evaluation on multi-column      
           .1D files, which 1deval also does not implement.  For example:   
                                                                            
            3dcalc -a '1D: 3 4 5 | 1 2 3'\' -expr 'cbrt(a)' -prefix -      
                                                                            
    ------------------------------------------------------------------------
    '1D:' INPUT:                                                            
    -----------                                                             
                                                                            
     You can input a 1D time series 'dataset' directly on the command line, 
     without an external file.  The 'filename for such input takes the      
     general format                                                         
                                                                            
       '1D:n_1@val_1,n_2@val_2,n_3@val_3,...'                               
                                                                            
     where each 'n_i' is an integer and each 'val_i' is a float.  For       
     example                                                                
                                                                            
        -a '1D:5@0,10@1,5@0,10@1,5@0'                                       
                                                                            
     specifies that variable 'a' be assigned to a 1D time series of 35,     
     alternating in blocks between values 0 and value 1.                    
    
     You can combine 3dUndump with 3dcalc to create an all zero 3D+time     
     dataset from 'thin air', as in the commands                            
        3dUndump -dimen 128 128 32 -prefix AllZero_A -datum float           
        3dcalc -a AllZero_A+orig -b '1D: 100@' -expr 0 -prefix AllZero_B    
     If you replace the '0' expression with 'gran(0,1)', you'd get a        
     random 3D+time dataset, which might be useful for testing purposes.    
                                                                            
    ------------------------------------------------------------------------
    'I:*.1D' and 'J:*.1D' and 'K:*.1D' INPUT:                               
    ----------------------------------------                                
                                                                            
     You can input a 1D time series 'dataset' to be defined as spatially    
     dependent instead of time dependent using a syntax like:               
                                                                            
       -c I:fred.1D                                                         
                                                                            
     This indicates that the n-th value from file fred.1D is to be associated
     with the spatial voxel index i=n (respectively j=n and k=n for 'J: and 
     K: input dataset names).  This technique can be useful if you want to  
     scale each slice by a fixed constant; for example:                     
                                                                            
       -a dset+orig -b K:slicefactor.1D -expr 'a*b'                         
                                                                            
     In this example, the '-b' value only varies in the k-index spatial     
     direction.                                                             
                                                                            
    ------------------------------------------------------------------------
    COORDINATES and PREDEFINED VALUES:                                      
    ---------------------------------                                       
                                                                            
     If you don't use '-x', '-y', or '-z' for a dataset, then the voxel     
     spatial coordinates will be loaded into those variables.  For example, 
     the expression 'a*step(x*x+y*y+z*z-100)' will zero out all the voxels  
     inside a 10 mm radius of the origin x=y=z=0.                           
                                                                            
     Similarly, the '-t' value, if not otherwise used by a dataset or *.1D  
     input, will be loaded with the voxel time coordinate, as determined    
     from the header file created for the OUTPUT.  Please note that the units
     of this are variable; they might be in milliseconds, seconds, or Hertz.
     In addition, slices of the dataset might be offset in time from one    
     another, and this is allowed for in the computation of 't'.  Use program
     3dinfo to find out the structure of your datasets, if you are not sure.
     If no input datasets are 3D+time, then the effective value of TR is    
     tstep in the output dataset, with t=0 at the first sub-brick.          
                                                                            
     Similarly, the '-i', '-j', and '-k' values, if not otherwise used,     
     will be loaded with the voxel spatial index coordinates.  The '-l'     
     (letter 'ell') value will be loaded with the temporal index coordinate.
                                                                            
     The '-n' value, if not otherwise used, will be loaded with the overall 
     voxel 1D index.  For a 3D dataset, n = i + j*NX + k*NX*NY, where       
     NX, NY, NZ are the array dimensions of the 3D grid.  [29 Jul 2010]     
                                                                            
     Otherwise undefined letters will be set to zero.  In the future, new   
     default values for other letters may be added.                         
                                                                            
     NOTE WELL: By default, the coordinate order of (x,y,z) is the order in 
     *********  which the data array is stored on disk; this order is output
                by 3dinfo.  The options below control can change this order:
                                                                            
     -dicom }= Sets the coordinates to appear in DICOM standard (RAI) order,
     -RAI   }= (the AFNI standard), so that -x=Right, -y=Anterior , -z=Inferior,
                                            +x=Left , +y=Posterior, +z=Superior.
                                                                            
     -SPM   }= Sets the coordinates to appear in SPM (LPI) order,           
     -LPI   }=                      so that -x=Left , -y=Posterior, -z=Inferior,
                                            +x=Right, +y=Anterior , +z=Superior.
                                                                            
     The -LPI/-RAI behavior can also be achieved via the AFNI_ORIENT        
     environment variable (27 Aug, 2014).                                   
    ------------------------------------------------------------------------
    DIFFERENTIAL SUBSCRIPTS [22 Nov 1999]:                                  
    -----------------------                                                 
                                                                            
     Normal calculations with 3dcalc are strictly on a per-voxel basis:
     there is no 'cross-talk' between spatial or temporal locations.
     The differential subscript feature allows you to specify variables
     that refer to different locations, relative to the base voxel.
     For example,
       -a fred+orig -b 'a[1,0,0,0]' -c 'a[0,-1,0,0]' -d 'a[0,0,2,0]'
     means: symbol 'a' refers to a voxel in dataset fred+orig,
            symbol 'b' refers to the following voxel in the x-direction,
            symbol 'c' refers to the previous voxel in the y-direction
            symbol 'd' refers to the 2nd following voxel in the z-direction
    
     To use this feature, you must define the base dataset (e.g., 'a')
     first.  Then the differentially subscripted symbols are defined
     using the base dataset symbol followed by 4 integer subscripts,
     which are the shifts in the x-, y-, z-, and t- (or sub-brick index)
     directions. For example,
    
       -a fred+orig -b 'a[0,0,0,1]' -c 'a[0,0,0,-1]' -expr 'median(a,b,c)'
    
     will produce a temporal median smoothing of a 3D+time dataset (this
     can be done more efficiently with program 3dTsmooth).
    
     Note that the physical directions of the x-, y-, and z-axes depend
     on how the dataset was acquired or constructed.  See the output of
     program 3dinfo to determine what direction corresponds to what axis.
    
     For convenience, the following abbreviations may be used in place of
     some common subscript combinations:
    
       [1,0,0,0] == +i    [-1, 0, 0, 0] == -i
       [0,1,0,0] == +j    [ 0,-1, 0, 0] == -j
       [0,0,1,0] == +k    [ 0, 0,-1, 0] == -k
       [0,0,0,1] == +l    [ 0, 0, 0,-1] == -l
    
     The median smoothing example can thus be abbreviated as
    
       -a fred+orig -b a+l -c a-l -expr 'median(a,b,c)'
    
     When a shift calls for a voxel that is outside of the dataset range,
     one of three things can happen:
    
       STOP => shifting stops at the edge of the dataset
       WRAP => shifting wraps back to the opposite edge of the dataset
       ZERO => the voxel value is returned as zero
    
     Which one applies depends on the setting of the shifting mode at the
     time the symbol using differential subscripting is defined.  The mode
     is set by one of the switches '-dsSTOP', '-dsWRAP', or '-dsZERO'.  The
     default mode is STOP.  Suppose that a dataset has range 0..99 in the
     x-direction.  Then when voxel 101 is called for, the value returned is
    
       STOP => value from voxel 99 [didn't shift past edge of dataset]
       WRAP => value from voxel 1  [wrapped back through opposite edge]
       ZERO => the number 0.0 
    
     You can set the shifting mode more than once - the most recent setting
     on the command line applies when a differential subscript symbol is
     encountered.
    
    N.B.: You can also use program 3dLocalstat to process data from a
          spatial neighborhood of each voxel; for example, to compute
          the maximum over a sphere of radius 9 mm placed around
          each voxel:
            3dLocalstat -nbhd 'SPHERE(9)' -stat max -prefix Amax9 A+orig
    
    ------------------------------------------------------------------------
    ISSUES:
    ------ 
    
     * Complex-valued datasets cannot be processed, except via '-cx2r'.
     * This program is not very efficient (but is faster than it once was).
     * Differential subscripts slow the program down even more.
    
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    EXPRESSIONS:
    ----------- 
    
     As noted above, datasets are referred to by single letter variable names.
     Arithmetic expressions are allowed, using + - * / ** ^ and parentheses.
     C relational, boolean, and conditional expressions are NOT implemented!
    * Note that the expression evaluator is designed not to fail;  illegal  *
    * operations like 'sqrt(-1)' are changed to legal ones to avoid crashes.*
     Built in functions include:
    
        sin  , cos  , tan  , asin  , acos  , atan  , atan2,       
        sinh , cosh , tanh , asinh , acosh , atanh , exp  ,       
        log  , log10, abs  , int   , sqrt  , max   , min  ,       
        J0   , J1   , Y0   , Y1    , erf   , erfc  , qginv, qg ,  
        rect , step , astep, bool  , and   , or    , mofn ,       
        sind , cosd , tand , median, lmode , hmode , mad  ,       
        gran , uran , iran , eran  , lran  , orstat, mod  ,       
        mean , stdev, sem  , Pleg  , cbrt  , rhddc2, hrfbk4,hrfbk5
        minabove, maxbelow, extreme, absextreme    , acfwxm
    
     where some of the less obvious funcions are:
     * qg(x)    = reversed cdf of a standard normal distribution
     * qginv(x) = inverse function to qg
     * min, max, atan2 each take 2 arguments ONLY
     * J0, J1, Y0, Y1 are Bessel functions (see the holy book: Watson)
     * Pleg(m,x) is the m'th Legendre polynomial evaluated at x
     * erf, erfc are the error and complementary error functions
     * sind, cosd, tand take arguments in degrees (vs. radians)
     * median(a,b,c,...) computes the median of its arguments
     * mad(a,b,c,...) computes the MAD of its arguments
     * mean(a,b,c,...) computes the mean of its arguments
     * stdev(a,b,c,...) computes the standard deviation of its arguments
     * sem(a,b,c,...) computes standard error of the mean of its arguments,
                      where sem(n arguments) = stdev(same)/sqrt(n)
     * orstat(n,a,b,c,...) computes the n-th order statistic of
        {a,b,c,...} - that is, the n-th value in size, starting
        at the bottom (e.g., orstat(1,a,b,c) is the minimum)
     * minabove(X,a,b,c,...) computes the smallest value amongst {a,b,c,...}
        that is LARGER than the first argument X; if all values are smaller
        than X, then X will be returned
     * maxbelow(X,a,b,c,...) similarly returns the largest value amongst
        {a,b,c,...} that is SMALLER than the first argument X.
     * extreme(a,b,c,...) finds the largest absolute value amongst
        {a,b,c,...} returning one of the original a,b,c,... values.
     * absextreme(a,b,c,...) finds the largest absolute value amongst
        {a,b,c,...} returning the maximum absolute value of a,b,c,... values.
     * lmode(a,b,c,...) and hmode(a,b,c,...) compute the mode
        of their arguments - lmode breaks ties by choosing the
        smallest value with the maximal count, hmode breaks ties by
        choosing the largest value with the maximal count
        ["a,b,c,..." indicates a variable number of arguments]
     * gran(m,s) returns a Gaussian deviate with mean=m, stdev=s
     * uran(r)   returns a uniform deviate in the range [0,r]
     * iran(t)   returns a random integer in the range [0..t]
     * eran(s)   returns an exponentially distributed deviate
                   with parameter s; mean=s
     * lran(t)   returns a logistically distributed deviate
                   with parameter t; mean=0, stdev=t*1.814
     * mod(a,b)  returns (a modulo b) = a - b*int(a/b)
     * hrfbk4(t,L) and hrfbk5(t,L) are the BLOCK4 and BLOCK5 hemodynamic
        response functions from 3dDeconvolve (L=stimulus duration in sec,
        and t is the time in sec since start of stimulus); for example:
     1deval -del 0.1 -num 400 -expr 'hrfbk5(t-2,20)' | 1dplot -stdin -del 0.1
        These HRF functions are scaled to return values in the range [0..1]
    
     * ACFWXM(a,b,c,x) returns the Full Width at X Maximum for the mixed
       model ACF function
         f(r) = a*expr(-r*r/(2*b*b))+(1-a)*exp(-r/c)
       for X between 0 and 1 (not inclusive).  This is the model function
       estimated in program 3dFWHMx.
    
     You may use the symbol 'PI' to refer to the constant of that name.
     This is the only 2 letter symbol defined; all variables are
     referred to by 1 letter symbols.  The case of the expression is
     ignored (in fact, it is converted to uppercase as the first step
     in the parsing algorithm).
    
     The following functions are designed to help implement logical
     functions, such as masking of 3D volumes against some criterion:
           step(x)    = {1 if x>0           , 0 if x<=0},
           posval(x)  = {x if x>0           , 0 if x<=0},
           astep(x,y) = {1 if abs(x) > y    , 0 otherwise} = step(abs(x)-y)
      within(x,MI,MX) = {1 if MI <= x <= MX , 0 otherwise},
           rect(x)    = {1 if abs(x)<=0.5, 0 if abs(x)>0.5},
           bool(x)    = {1 if x != 0.0   , 0 if x == 0.0},
        notzero(x)    = bool(x),
         iszero(x)    = 1-bool(x) = { 0 if x != 0.0, 1 if x == 0.0 },
            not(x)    = same as iszero(x)
         equals(x,y)  = 1-bool(x-y) = { 1 if x == y , 0 if x != y },
       ispositive(x)  = { 1 if x > 0; 0 if x <= 0 },
       isnegative(x)  = { 1 if x < 0; 0 if x >= 0 },
       ifelse(x,t,f)  = { t if x != 0; f if x == 0 },
            not(x)    = same as iszero(x) = Boolean negation
       and(a,b,...,c) = {1 if all arguments are nonzero, 0 if any are zero}
        or(a,b,...,c) = {1 if any arguments are nonzero, 0 if all are zero}
      mofn(m,a,...,c) = {1 if at least 'm' arguments are nonzero, else 0 }
      argmax(a,b,...) = index of largest argument; = 0 if all args are 0
      argnum(a,b,...) = number of nonzero arguments
      pairmax(a,b,...)= finds the 'paired' argument that corresponds to the
                        maximum of the first half of the input arguments;
                        for example, pairmax(a,b,c,p,q,r) determines which
                        of {a,b,c} is the max, then returns corresponding
                        value from {p,q,r}; requires even number of args.
      pairmin(a,b,...)= Similar to pairmax, but for minimum; for example,
                        pairmin(a,b,c,p,q,r} finds the minimum of {a,b,c}
                        and returns the corresponding value from {p,q,r};
                          pairmin(3,2,7,5,-1,-2,-3,-4) = -2
                        (The 'pair' functions are Lukas Pezawas specials!)
      amongst(a,b,...)= Return value is 1 if any of the b,c,... values
                        equals the a value; otherwise, return value is 0.
     choose(n,a,b,...)= chooses the n-th value from the a,b,... values.
                        (e.g., choose(2,a,b,c) is b)
    
      [These last 9 functions take a variable number of arguments.]
    
     The following 27 functions are used for statistical conversions,
     as in the program 'cdf':
       fico_t2p(t,a,b,c), fico_p2t(p,a,b,c), fico_t2z(t,a,b,c),
       fitt_t2p(t,a)    , fitt_p2t(p,a)    , fitt_t2z(t,a)    ,
       fift_t2p(t,a,b)  , fift_p2t(p,a,b)  , fift_t2z(t,a,b)  ,
       fizt_t2p(t)      , fizt_p2t(p)      , fizt_t2z(t)      ,
       fict_t2p(t,a)    , fict_p2t(p,a)    , fict_t2z(t,a)    ,
       fibt_t2p(t,a,b)  , fibt_p2t(p,a,b)  , fibt_t2z(t,a,b)  ,
       fibn_t2p(t,a,b)  , fibn_p2t(p,a,b)  , fibn_t2z(t,a,b)  ,
       figt_t2p(t,a,b)  , figt_p2t(p,a,b)  , figt_t2z(t,a,b)  ,
       fipt_t2p(t,a)    , fipt_p2t(p,a)    , fipt_t2z(t,a)    .
    
     See the output of 'cdf -help' for documentation on the meanings of
     and arguments to these functions.  The two functions below use the
     NIfTI-1 statistical codes to map between statistical values and
     cumulative distribution values:
       cdf2stat(val,code,p1,p2,p3) -- val is between 0 and 1
       stat2cdf(val,code,p1,p2,p3) -- val is legal for the given distribution
     where code is
       2 = correlation statistic     p1 = DOF
       3 = t statistic (central)     p1 = DOF
       4 = F statistic (central)     p1 = num DOF, p2 = den DOF
       5 = N(0,1) statistic          no parameters (p1=p2=p3=0)
       6 = Chi-squared (central)     p1 = DOF
       7 = Beta variable (central)   p1 = a , p2 = b
       8 = Binomial variable         p1 = #trials, p2 = prob per trial
       9 = Gamma distribution        p1 = shape, p2 = scale
      10 = Poisson distribution      p1 = mean
      11 = N(mu,variance) normal     p1 = mean, p2 = scale
      12 = noncentral F statistic    p1 = num DOF, p2 = den DOF, p3 = noncen
      13 = noncentral chi-squared    p1 = DOF, p2 = noncentrality parameter
      14 = Logistic distribution     p1 = mean, p2 = scale
      15 = Laplace distribution      p1 = mean, p2 = scale
      16 = Uniform distribution      p1 = min, p2 = max
      17 = noncentral t statistic    p1 = DOF, p2 = noncentrality parameter
      18 = Weibull distribution      p1 = location, p2 = scale, p3 = power
      19 = Chi statistic (central)   p1 = DOF
      20 = inverse Gaussian variable p1 = mu, p2 = lambda
      21 = Extreme value type I      p1 = location, p2 = scale
      22 = 'p-value'                 no parameters
      23 = -ln(p)                    no parameters
      24 = -log10(p)                 no parameters
    When fewer than 3 parameters are needed, the values for later parameters
    are still required, but will be ignored.  An extreme case is code=5,
    where the correct call is (e.g.) cdf2stat(p,5,0,0,0)
    
    Finally, note that the expression evaluator is designed not to crash, or
    to return NaN or Infinity.  Illegal operations, such as division by 0,
    logarithm of negative value, etc., are intercepted and something else
    (usually 0) will be returned.  To find out what that 'something else'
    is in any specific case, you should play with the ccalc program.
    
    ** If you modify a statistical sub-brick, you may want to use program
      '3drefit' to modify the dataset statistical auxiliary parameters.
    
    ** Computations are carried out in double precision before being
       truncated to the final output 'datum'.
    
    ** Note that the quotes around the expression are needed so the shell
       doesn't try to expand * characters, or interpret parentheses.
    
    ** Try the 'ccalc' program to see how the expression evaluator works.
       The arithmetic parser and evaluator is written in Fortran-77 and
       is derived from a program written long ago by RW Cox to facilitate
       compiling on an array processor hooked up to a VAX. (It's a mess, but
       it works - somewhat slowly - but hey, computers are fast these days.)
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
