*********
3dDetrend
*********

.. _ahelp_3dDetrend:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dDetrend [options] dataset
    * This program removes components from voxel time series using
      linear least squares.  Each voxel is treated independently.
    * Note that least squares detrending is equivalent to orthogonalizing
      the input dataset time series with respect to the basis time series
      provided by the '-vector', '-polort', et cetera options.
    * The input dataset may have a sub-brick selector string; otherwise,
      all sub-bricks will be used.
    
    
      *** You might also want to consider using program 3dBandpass ***
    General Options:
     -prefix pname = Use 'pname' for the output dataset prefix name.
                       [default='detrend']
     -session dir  = Use 'dir' for the output dataset session directory.
                       [default='./'=current working directory]
     -verb         = Print out some verbose output as the program runs.
     -replace      = Instead of subtracting the fit from each voxel,
                       replace the voxel data with the time series fit.
     -normalize    = Normalize each output voxel time series; that is,
                       make the sum-of-squares equal to 1.
               N.B.: This option is only valid if the input dataset is
                       stored as floats! (1D files are always floats.)
     -byslice      = Treat each input vector (infra) as describing a set of
                       time series interlaced across slices.  If NZ is the
                       number of slices and NT is the number of time points,
                       then each input vector should have NZ*NT values when
                       this option is used (usually, they only need NT values).
                       The values must be arranged in slice order, then time
                       order, in each vector column, as shown here:
                           f(z=0,t=0)       // first slice, first time
                           f(z=1,t=0)       // second slice, first time
                           ...
                           f(z=NZ-1,t=0)    // last slice, first time
                           f(z=0,t=1)       // first slice, second time
                           f(z=1,t=1)       // second slice, second time
                           ...
                           f(z=NZ-1,t=NT-1) // last slice, last time
    
    Component Options:
    These options determine the components that will be removed from
    each dataset voxel time series.  They may be repeated to specify
    multiple regression.  At least one component must be specified.
    
     -vector vvv   = Remove components proportional to the columns vectors
                       of the ASCII *.1D file 'vvv'.  You may use a
                       sub-vector selector string to specify which columns
                       to use; otherwise, all columns will be used.
                       For example:
                        -vector 'xyzzy.1D[3,5]'
                       will remove the 4th and 6th columns of file xyzzy.1D
                       from the dataset (sub-vector indexes start at 0).
                     You can use multiple -vector instances to specify
                       components from different files.
    
     -expr eee     = Remove components proportional to the function
                       specified in the expression string 'eee'.
                       Any single letter from a-z may be used as the
                       independent variable in 'eee'.  For example:
                        -expr 'cos(2*PI*t/40)' -expr 'sin(2*PI*t/40)'
                       will remove sine and cosine waves of period 40
                       from the dataset.
    
     -polort ppp   = Add Legendre polynomials of order up to and
                       including 'ppp' in the list of vectors to remove.
    
     -del ddd      = Use the numerical value 'ddd' for the stepsize
                       in subsequent -expr options.  If no -del option
                       is ever given, then the TR given in the dataset
                       header is used for 'ddd'; if that isn't available,
                       then 'ddd'=1.0 is assumed.  The j-th time point
                       will have independent variable = j * ddd, starting
                       at j=0.  For example:
                         -expr 'sin(x)' -del 2.0 -expr 'z**3'
                       means that the stepsize in 'sin(x)' is delta-x=TR,
                       but the stepsize in 'z**3' is delta-z = 2.
    
     N.B.: expressions are NOT calculated on a per-slice basis when the
            -byslice option is used.  If you have to do this, you could
            compute vectors with the required time series using 1deval.
    
    Detrending 1D files
    -------------------
    As far as '3d' programs are concerned, you can input a 1D file as
    a 'dataset'.  Each row is a separate voxel, and each column is a
    separate time point.  If you want to detrend a single column, then
    you need to transpose it on input.  For example:
    
      3dDetrend -prefix - -vector G1.1D -polort 3 G5.1D\' | 1dplot -stdin
    
    Note that the '-vector' file is NOT transposed with \', but that
    the input dataset file IS transposed.  This is because in the first
    case the program expects a 1D file, and so knows that the column
    direction is time.  In the second case, the program expects a 3D
    dataset, and when given a 1D file, knows that the row direction is
    time -- so it must be transposed.  I'm sorry if this is confusing,
    but that's the way it is.
    
    NOTE: to have the output file appear so that time is in the column
          direction, you'll have to add the option '-DAFNI_1D_TRANOUT=YES'
          to the command line, as in
    
      3dDetrend -DAFNI_1D_TRANOUT=YES -prefix - -vector G1.1D -polort 3 G5.1D\' > Q.1D
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
