*********
3dmaskSVD
*********

.. _ahelp_3dmaskSVD:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage:  3dmaskSVD [options] inputdataset
    Author: Zhark the Gloriously Singular
    
    * Computes the principal singular vector of the time series
        vectors extracted from the input dataset over the input mask.
      ++ You can use the '-sval' option to change which singular
         vectors are output.
    * The sign of the output vector is chosen so that the average
        of arctanh(correlation coefficient) over all input data
        vectors (from the mask) is positive.
    * The output vector is normalized: the sum of its components
        squared is 1.
    * You probably want to use 3dDetrend (or something similar) first,
        to get rid of annoying artifacts, such as motion, breathing,
        dark matter interactions with the brain, etc.
      ++ If you are lazy scum like Zhark, you might be able to get
         away with using the '-polort' option.
      ++ In particular, if your data time series has a nonzero mean,
         then you probably want at least '-polort 0' to remove the
         mean, otherwise you'll pretty much just get a constant
         time series as the principal singular vector!
    * An alternative to this program would be 3dmaskdump followed
        by 1dsvd, which could give you all the singular vectors you
        could ever want, and much more -- enough to confuse you for days.
      ++ In particular, although you COULD input a 1D file into
         3dmaskSVD, the 1dsvd program would make much more sense.
    * This program will be pretty slow if there are over about 2000
        voxels in the mask.  It could be made more efficient for
        such cases, but you'll have to give Zhark some 'incentive'.
    * Result vector goes to stdout.  Redirect per your pleasures and needs.
    * Also see program 3dLocalSVD if you want to compute the principal
        singular time series vector from a neighborhood of EACH voxel.
      ++ (Which is a pretty slow operation!)
    * http://en.wikipedia.org/wiki/Singular_value_decomposition
    
    -------
    Options:
    -------
     -vnorm      = L2 normalize all time series before SVD [recommended!]
     -sval a     = output singular vectors 0 .. a [default a=0 = first one only]
     -mask mset  = define the mask [default is entire dataset == slow!]
     -automask   = you'll have to guess what this option does
     -polort p   = if you are lazy and didn't run 3dDetrend (like Zhark)
     -bpass L H  = bandpass [mutually exclusive with -polort]
     -ort xx.1D  = time series to remove from the data before SVD-ization
                   ++ You can give more than 1 '-ort' option
                   ++ 'xx.1D' can contain more than 1 column
     -input ddd  = alternative way to give the input dataset name
    
    -------
    Example:
    -------
     You have a mask dataset with discrete values 1, 2, ... 77 indicating
     some ROIs; you want to get the SVD from each ROI's time series separately,
     and then put these into 1 big 77 column .1D file.  You can do this using
     a csh shell script like the one below:
    
     # Compute the individual SVD vectors
     foreach mm ( `count 1 77` )
       3dmaskSVD -vnorm -mask mymask+orig"<${mm}..${mm}>" epi+orig > qvec${mm}.1D
     end
     # Glue them together into 1 big file, then delete the individual files
     1dcat qvec*.1D > allvec.1D
     /bin/rm -f qvec*.1D
     # Plot the results to a JPEG file, then compute their correlation matrix
     1dplot -one -nopush -jpg allvec.jpg allvec.1D
     1ddot -terse allvec.1D > allvec_COR.1D
    
     [[ If you use the bash shell,  you'll have to figure out the syntax ]]
     [[ yourself. Zhark has no sympathy for you bash shell infidels, and ]]
     [[ considers you only slightly better than those lowly Emacs users. ]]
     [[ And do NOT ever even mention 'nedit' in Zhark's august presence! ]]
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
