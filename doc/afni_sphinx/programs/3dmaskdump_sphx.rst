.. contents:: 
    :depth: 4 

**********
3dmaskdump
**********

.. code-block:: none

    Usage: 3dmaskdump [options] dataset dataset ...
    Writes to an ASCII file values from the input datasets
    which satisfy the mask criteria given in the options.
    If no options are given, then all voxels are included.
    This might result in a GIGANTIC output file.
    Options:
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be printed from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
      -mrange a b  Means to further restrict the voxels from
                     'mset' so that only those mask values
                     between 'a' and 'b' (inclusive) will
                     be used.  If this option is not given,
                     all nonzero values from 'mset' are used.
                     Note that if a voxel is zero in 'mset', then
                     it won't be included, even if a < 0 < b.
      -index       Means to write out the dataset index values.
      -noijk       Means not to write out the i,j,k values.
      -xyz         Means to write the x,y,z coordinates from
                     the 1st input dataset at the start of each
                     output line.  These coordinates are in
                     the 'RAI' (DICOM) order.
      -o fname     Means to write output to file 'fname'.
                     [default = stdout, which you won't like]
    
      -cmask 'opts' Means to execute the options enclosed in single
                      quotes as a 3dcalc-like program, and produce
                      produce a mask from the resulting 3D brick.
           Examples:
            -cmask '-a fred+orig[7] -b zork+orig[3] -expr step(a-b)'
                      produces a mask that is nonzero only where
                      the 7th sub-brick of fred+orig is larger than
                      the 3rd sub-brick of zork+orig.
            -cmask '-a fred+orig -expr 1-bool(k-7)'
                      produces a mask that is nonzero only in the
                      7th slice (k=7); combined with -mask, you
                      could use this to extract just selected voxels
                      from particular slice(s).
           Notes: * You can use both -mask and -cmask in the same
                      run - in this case, only voxels present in
                      both masks will be dumped.
                  * Only single sub-brick calculations can be
                      used in the 3dcalc-like calculations -
                      if you input a multi-brick dataset here,
                      without using a sub-brick index, then only
                      its 0th sub-brick will be used.
                  * Do not use quotes inside the 'opts' string!
    
      -xbox x y z   Means to put a 'mask' down at the dataset (not DICOM)
                      coordinates of 'x y z' mm.  By default, this box is
                      1 voxel wide in each direction.  You can specify
                      instead a range of coordinates using a colon ':'
                      after the coordinates; for example:
                        -xbox 22:27 31:33 44
                      means a box from (x,y,z)=(22,31,44) to (27,33,44).
               NOTE: dataset coordinates are NOT the coordinates you
                     typically see in AFNI's main controller top left corner.
                     Those coordinates are typically in either RAI/DICOM order
                     or in LPI/SPM order and should be used with -dbox and
                     -nbox, respectively.
    
      -dbox x y z   Means the same as -xbox, but the coordinates are in
                      RAI/DICOM order (+x=Left, +y=Posterior, +z=Superior).
                      If your AFNI environment variable AFNI_ORIENT is set to
                      RAI, these coordinates correspond to those you'd enter
                      into the 'Jump to (xyz)' control in AFNI, and to
                      those output by 3dclust.
                NOTE: It is possible to make AFNI and/or 3dclust output 
                      coordinates in an order different from the one specified 
                      by AFNI_ORIENT, but you'd have to work hard on that. 
                      In any case, the order is almost always specified along 
                      with the coordinates. If you see RAI/DICOM, then use 
                      -dbox. If you see LPI/SPM then use -nbox. 
    
      -nbox x y z   Means the same as -xbox, but the coordinates are in
                      LPI/SPM or 'neuroscience' order where the signs of the
                      x and y coordinates are reversed relative to RAI/DICOM.
                      (+x=Right, +y=Anterior, +z=Superior)
    
      -ibox i j k   Means to put a 'mask' down at the voxel indexes
                      given by 'i j k'.  By default, this picks out
                      just 1 voxel.  Again, you can use a ':' to specify
                      a range (now in voxels) of locations.
           Notes: * Boxes are cumulative; that is, if you specify more
                      than 1 box, you'll get more than one region.
                  * If a -mask and/or -cmask option is used, then
                      the INTERSECTION of the boxes with these masks
                      determines which voxels are output; that is,
                      a voxel must be inside some box AND inside the
                      mask in order to be selected for output.
                  * If boxes select more than 1 voxel, the output lines
                      are NOT necessarily in the order of the options on
                      the command line.
                  * Coordinates (for -xbox, -dbox, and -nbox) are relative
                      to the first dataset on the command line.
    
      -xball x y z r  Means to put a ball (sphere) mask down at dataset
                        coordinates (x,y,z) with radius r.
      -dball x y z r  Same, but (x,y,z) are in RAI/DICOM order.
      -nball x y z r  Same, but (x,y,z) are in LPI/SPM order.
           Notes: * The combined (set UNION) of all ball and/or box masks
                    is created first.  Then, if a -mask and/or -cmask
                    option was used, then the ball+box mask will be
                    INTERSECTED with the existing mask.
    
      -nozero       Means to skip output of any voxel where all the
                      data values are zero.
    
      -n_rand N_RAND Means to keep only N_RAND randomly selected
                     voxels from what would have been the output.
    
      -n_randseed SEED  Seed the random number generator with SEED,
                        instead of the default seed of 1234
    
      -niml name    Means to output data in the XML/NIML format that
                      is compatible with input back to AFNI via
                      the READ_NIML_FILE command.
                  * 'name' is the 'target_name' for the NIML header
                      field, which is the name that will be assigned
                      to the dataset when it is sent into AFNI.
                  * Also implies '-noijk' and '-xyz' and '-nozero'.
    
      -quiet        Means not to print progress messages to stderr.
    
    Inputs after the last option are datasets whose values you
    want to be dumped out.  These datasets (and the mask) can
    use the sub-brick selection mechanism (described in the
    output of '3dcalc -help') to choose which values you get.
    
    Each selected voxel gets one line of output:
      i j k val val val ....
    where (i,j,k) = 3D index of voxel in the dataset arrays,
    and val = the actual voxel value.  Note that if you want
    the mask value to be output, you have to include that
    dataset in the dataset input list again, after you use
    it in the '-mask' option.
    
    * To eliminate the 'i j k' columns, use the '-noijk' option.
    * To add spatial coordinate columns, use the '-xyz' option.
    
    N.B.: This program doesn't work with complex-valued datasets!
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
