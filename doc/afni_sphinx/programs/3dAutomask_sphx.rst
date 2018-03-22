**********
3dAutomask
**********

.. _3dAutomask:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAutomask [options] dataset
    Input dataset is EPI 3D+time, or a skull-stripped anatomical.
    Output dataset is a brain-only mask dataset.
    
    This program by itself does NOT do 'skull-stripping'.  Use
    program 3dSkullStrip for that purpose!
    
    Method:
     + Uses 3dClipLevel algorithm to find clipping level.
     + Keeps only the largest connected component of the
       supra-threshold voxels, after an erosion/dilation step.
     + Writes result as a 'fim' type of functional dataset,
       which will be 1 inside the mask and 0 outside the mask.
    
    Options:
    --------
      -prefix ppp = Write mask into dataset with prefix 'ppp'.
                     [Default == 'automask']
    
      -apply_prefix ppp = Apply mask to input dataset and save
                    masked dataset. If an apply_prefix is given
                    and not the usual prefix, the only output
                    will be the applied dataset
    
      -clfrac cc  = Set the 'clip level fraction' to 'cc', which
                     must be a number between 0.1 and 0.9.
                     A small 'cc' means to make the initial threshold
                     for clipping (a la 3dClipLevel) smaller, which
                     will tend to make the mask larger.  [default=0.5]
    
      -nograd     = The program uses a 'gradual' clip level by default.
                     To use a fixed clip level, use '-nograd'.
                     [Change to gradual clip level made 24 Oct 2006.]
    
      -peels pp   = Peel the mask 'pp' times, then unpeel.  Designed
                     to clip off protuberances less than 2*pp voxels
                     thick. [Default == 1]
    
      -nbhrs nn   = Define the number of neighbors needed for a voxel
                     NOT to be peeled.  The 18 nearest neighbors in
                     the 3D lattice are used, so 'nn' should be between
                     9 and 18.  [Default == 17]
    
      -q          = Don't write progress messages (i.e., be quiet).
    
      -eclip      = After creating the mask, remove exterior
                     voxels below the clip threshold.
    
      -dilate nd  = Dilate the mask outwards 'nd' times.
    
      -erode ne   = Erode the mask inwards 'ne' times.
    
      -SI hh      = After creating the mask, find the most superior
                     voxel, then zero out everything more than 'hh'
                     millimeters inferior to that.  hh=130 seems to
                     be decent (i.e., for Homo Sapiens brains).
    
      -depth DEP  = Produce a dataset (DEP) that shows how many peel 
                    operations it takes to get to a voxel in the mask.
                    The higher the number, the deeper a voxel is located 
                    in the mask. 
              None of -peels, -dilate, or -erode affect this option.
    --------------------------------------------------------------------
    How to make an edge-of-brain mask from an anatomical volume:
    * 3dSkullStrip to create a brain-only dataset; say, Astrip+orig
    * 3dAutomask -prefix Amask Astrip+orig
    * Create a mask of edge-only voxels via
       3dcalc -a Amask+orig -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k \
              -expr 'ispositive(a)*amongst(0,b,c,d,e,f,g)' -prefix Aedge
      which will be 1 at all voxels in the brain mask that have a
      nearest neighbor that is NOT in the brain mask.
    * cf. '3dcalc -help' DIFFERENTIAL SUBSCRIPTS for information
      on the 'a+i' et cetera inputs used above.
    * In regions where the brain mask is 'stair-stepping', then the
      voxels buried inside the corner of the steps probably won't
      show up in this edge mask:
         ...00000000...
         ...aaa00000...
         ...bbbaa000...
         ...bbbbbaa0...
      Only the 'a' voxels are in this edge mask, and the 'b' voxels
      down in the corners won't show up, because they only touch a
      0 voxel on a corner, not face-on.  Depending on your use for
      the edge mask, this effect may or may not be a problem.
    --------------------------------------------------------------------
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
