*********
3dUnifize
*********

.. _3dUnifize:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dUnifize [options] inputdataset
    
    * The input dataset is supposed to be a T1-weighted volume,
      possibly already skull-stripped (e.g., via 3dSkullStrip).
      ++ However, this program can be a useful step to take BEFORE
         3dSkullStrip, since the latter program can fail if the input
         volume is strongly shaded -- 3dUnifize will (mostly) remove
         such shading artifacts.
    
    * The output dataset has the white matter (WM) intensity approximately
      uniformized across space, and scaled to peak at about 1000.
    
    * The output dataset is always stored in float format!
    
    * If the input dataset has more than 1 sub-brick, only sub-brick
      #0 will be processed!
    
    * If you have a lot of tissue inferior to the brain, you might have
      to cut it off (using 3dZeropad -I -xxx to cut off the most inferior
      xxx slices -- where you pick the number xxx visually), before
      using 3dUnifize.
    
    * Want to correct EPI datasets for nonuniformity?
      You can try the new and experimental [Mar 2017] '-EPI' option.
    
    * Method: Obi-Wan's personal variant of Ziad's sneaky trick.
      (If you want to know what his trick is, you'll have to ask him, or
       read Obi-Wan's source code [which is a world of ecstasy and exaltation],
       or just read all the way to the end of this help output.)
    
    * The principal motive for this program is for use in an image
      registration script, and it may or may not be useful otherwise.
    
    * This program replaces the older (and very different) 3dUniformize,
      which is no longer maintained and may sublimate at any moment.
      (In other words, we do not recommend the use of 3dUniformize.)
    
    --------
    Options:
    --------
    
      -prefix pp = Use 'pp' for prefix of output dataset.
    
      -input dd  = Alternative way to specify input dataset.
    
      -T2        = Treat the input as if it were T2-weighted, rather than
                   T1-weighted. This processing is done simply by inverting
                   the image contrast, processing it as if that result were
                   T1-weighted, and then re-inverting the results.
                  ++ This option is NOT guaranteed to be useful for anything!
                  ++ Of course, nothing in AFNI comes with a guarantee :-)
                  ++ If you want to be REALLY sneaky, giving this option twice
                     will skip the second inversion step, so the result will
                     look like a T1-weighted volume (except at the edges and
                     near blood vessels).
                  ++ Might be useful for skull-stripping T2-weighted datasets.
                  ++ Don't try the '-T2 -T2' trick on FLAIR-T2-weighted datasets.
                     The results aren't pretty!
    
      -GM        = Also scale to unifize 'gray matter' = lower intensity voxels
                   (to aid in registering images from different scanners).
                  ++ For many datasets (especially those created by averaging),
                     using '-GM' will increase the WM-GM contrast somewhat;
                     however, that depends on the original WM-GM contrast.
                  ++ This option is recommended for use with 3dQwarp when
                     aligning 2 T1-weighted volumes, in order to make the
                     WM-GM contrast about the same for the datasets, even
                     if they don't come from the same scanner/pulse-sequence.
                  ++ Note that standardizing the contrasts with 3dUnifize will help
                     3dQwarp match the source dataset to the base dataset.  If you
                     later want the original source dataset to be warped, you can
                     do so using the 3dNwarpApply program.
                  ++ In particular, the template dataset MNI152_2009_template.nii.gz
                     (supplied with AFNI) has been treated with '-GM'. This dataset
                     is the one used by the @SSwarper script, so that script applies
                     3dUnifize with this '-GM' option to help with the alignment.
    
      -Urad rr   = Sets the radius (in voxels) of the ball used for the sneaky trick.
                   ++ Default value is 18.3, and should be changed proportionally
                      if the dataset voxel size differs significantly from 1 mm.
    
      -ssave ss  = Save the scale factor used at each voxel into a dataset 'ss'.
                   ++ This is the white matter scale factor, and does not include
                      the factor from the '-GM' option (if that was included).
                   ++ The input dataset is multiplied by the '-ssave' image
                      (voxel-wise) to get the WM-unifized image.
                   ++ Another volume (with the same grid dimensions) could be
                      scaled the same way using 3dcalc, if that is needed.
                   ++ This saved scaled factor does NOT include any GM scaling :(
    
      -quiet     = Don't print the fun fun fun progress messages (but whyyyy?).
                   ++ For the curious, the codes used are:
                       A = Automask
                       D = Duplo down (process a half-size volume)
                       V = Voxel-wise histograms to get local scale factors
                       U = duplo Up (convert local scale factors to full-size volume)
                       W = multiply by White matter factors
                       G = multiply by Gray matter factors [cf the -GM option]
                       I = contrast inversion              [cf the -T2 option]
                       M = compute median volume           [for the -EPI option]
                       E = compute scaled EPI datasets     [for the -EPI option]
                   ++ 'Duplo down' means to scale the input volume to be half the
                      grid size in each direction for speed when computing the
                      voxel-wise histograms.  The sub-sampling is done using the
                      median of the central voxel value and its 6 nearest neighbors.
    
      -noduplo   = Do NOT use the 'duplo down' step; this can be useful for lower
                   resolution datasets.
                   ++ If a dataset has less than 1 million voxels in a 3D volume,
                      'duplo down' will not be used.
    
      -EPI       = Assume the input dataset is a T2 (or T2*) weighted EPI time
                   series. After computing the scaling, apply it to ALL volumes
                   (TRs) in the input dataset. That is, a given voxel will be
                   scaled by the same factor at each TR.
                   ++ This option also implies '-noduplo' and '-T2'.
                   ++ This option turns off '-GM' if you turned it on.
               -->>++ This option is experimental; check your results!
                   ++ Remember: the program tries to uniform-ize the White Matter
                      regions, so the overall appearance of the image may become
                      less uniform, especially if it was fairly uniform already.
                   ++ For most purposes in AFNI processing, uniform-izing
                      EPI datasets is not needed.
                      -- If you are having trouble getting a good result from
                         3dAutomask, try adding the option '-clfrac 0.2'.
                      -- There is no reason to apply 3dUnifize to EPI datasets
                         that do not have significant shading artifacts.
                      -- EPI data from 7T systems might be 'improved' by 3dUnifize.
                      -- You might need to run 3dDespike before using 3dUnifize.
    
    ------------------------------------------
    Special options for Jedi AFNI Masters ONLY:
    ------------------------------------------
      -rbt R b t = Specify the 3 parameters for the algorithm, as 3 numbers
                   following the '-rbt':
                     R = radius; same as given by option '-Urad'     [default=18.3]
                     b = bottom percentile of normalizing data range [default=70.0]
                     r = top percentile of normalizing data range    [default=80.0]
    
      -T2up uu   = Set the upper percentile point used for T2-T1 inversion.
                   The default value is 98.5 (for no good reason), and 'uu' is
                   allowed to be anything between 90 and 100 (inclusive).
                   ++ The histogram of the data is built, and the uu-th percentile
                      point value is called 'U'. The contrast inversion is simply
                      given by output_value = max( 0 , U - input_value ).
    
      -clfrac cc = Set the automask 'clip level fraction' to 'cc', which
                   must be a number between 0.1 and 0.9.
                   A small 'cc' means to make the initial threshold
                   for clipping (a la 3dClipLevel) smaller, which
                   will tend to make the mask larger.  [default=0.1]
                   ++ [22 May 2013] The previous version of this program used a
                      clip level fraction of 0.5, which proved to be too large
                      for some users, who had images with very strong shading issues.
                      Thus, the default value for this parameter was lowered to 0.1.
                   ++ [24 May 2016] The default value for this parameter was
                      raised to 0.2, since the lower value often left a lot of
                      noise outside the head on non-3dSkullStrip-ed datasets.
                      You can still manually set -clfrac to 0.1 if you need to
                      correct for very large shading artifacts.
                   ++ If the results of 3dUnifize have a lot of noise outside the head,
                      then using '-clfrac 0.5' (or even larger) will probably help.
    
    -- Feb 2013 - by Obi-Wan Unifobi
                - can always be found at the Everest Bakery in Namche Bazaar,
                  if you have any questions about this program
    -- This code uses OpenMP to speed up the slowest part (voxel-wise histograms).
    
    ----------------------------------------------------------------------------
    HOW IT WORKS (Ziad's sneaky trick is revealed at last! And more.)
    ----------------------------------------------------------------------------
    The basic idea is that white matter in T1-weighted images is reasonably
    uniform in intensity, at least when averaged over 'large-ish' regions.
    
    The first step is to create a local white matter intensity volume.
    Around each voxel (inside the volume 'automask'), the ball of values
    within a fixed radius (default=18.3 voxels) is extracted and these
    numbers are sorted.  The values in the high-intensity range of the
    histogram (default=70% to 80%) are averaged.  The result from this
    step is a smooth 3D map of the 'white matter intensity' (WMI).
    
     [The parameters of the above process can be altered with the '-rbt' option.]
     [For speed, the WMI map is produced on an image that is half-size in all   ]
     [directions ('Duplo down'), and then is expanded back to the full-size     ]
     [volume ('Duplo up').  The automask procedure can be somewhat controlled   ]
     [via the '-clfrac' option.  The default setting is designed to deal with   ]
     [heavily shaded images, where the WMI varies by a factor of 5 or more over ]
     [the image volume.                                                         ]
    
    The second step is to scale the value at every voxel location x in the input
    volume by the factor 1000/WMI(x), so that the 'white matter intensity' is
    now uniform-ized to be 1000 everywhere.  (This is Ziad's 'trick'; it is easy,
    works well, and doesn't require fitting some spatial model to the data: the
    data provides its own model.)
    
    If the '-GM' option is used, then this scaled volume is further processed
    to make the lower intensity values (presumably gray matter) have a contrast
    similar to that from a collection of 3 Tesla MP-RAGE images that were
    acquired at the NIH.  (This procedure is not Ziad's fault, and should be
    blamed on the reclusive Obi-Wan Unifobi.)
    
    From the WM-uniform-ized volume, the median of all values larger than 1000
    is computed; call this value P.  P-1000 represents the upward dispersion
    of the high-intensity (white matter) voxels in the volume.  This value is
    'reflected' below 1000 to Q = 1000 - 2*(P-1000), and Q is taken to be the
    upper bound for gray matter voxel intensities.  A lower bound for gray
    matter voxel values is estimated via the 'clip fraction' algorithm as
    implemented in program 3dClipLevel; call this lower bound R.  The median
    of all values between R and Q is computed; call this value G, which is taken
    to be a 'typical' gray matter voxel instensity.  Then the values z in the
    entire volume are linearly scaled by the formula
       z_out = (1000-666)/(1000-G) * (z_in-1000) + 1000
    so that the WM uniform-ized intensity of 1000 remains at 1000, and the gray
    matter median intensity of G is mapped to 666.  (Values z_out that end up
    negative are set to 0; as a result, some of CSF might end up as 0.)
    The value 666 was chosen because it gave results visually comparable to
    various NIH-generated 3 Tesla T1-weighted datasets.  (Any suggestions that
    this value was chosen for other reasons will be treated as 'beastly'.)
    
    To recap: the WM uniform-ization process provides a linear scaling factor
    that varies for each voxel ('local'), while the GM normalization process
    uses a global linear scaling.  The GM process is optional, and is simply
    designed to make the various T1-weighted images look similar.
    
    -----** CAVEAT **-----
    This procedure was primarily developed to aid in 3D registration, especially
    when using 3dQwarp, so that the registration algorithms are trying to match
    images that are alike.  It is *NOT* intended to be used for quantification
    purposes, such as Voxel Based Morphometry!  That would better be done via
    the 3dSeg program, which is far more complicated.
    ----------------------------------------------------------------------------
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
