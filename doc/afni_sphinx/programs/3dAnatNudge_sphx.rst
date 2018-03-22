***********
3dAnatNudge
***********

.. _ahelp_3dAnatNudge:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAnatNudge [options]
    Moves the anat dataset around to best overlap the epi dataset.
    
    OPTIONS:
     -anat aaa   = aaa is an 'scalped' (3dIntracranial) high-resolution
                    anatomical dataset [a mandatory option]
     -epi eee    = eee is an EPI dataset [a mandatory option]
                    The first [0] sub-brick from each dataset is used,
                    unless otherwise specified on the command line.
     -prefix ppp = ppp is the prefix of the output dataset;
                    this dataset will differ from the input only
                    in its name and its xyz-axes origin
                    [default=don't write new dataset]
     -step sss   = set the step size to be sss times the voxel size
                    in the anat dataset [default=1.0]
     -x nx       = search plus and minus nx steps along the EPI
     -y ny          dataset's x-axis; similarly for ny and the
     -z nz          y-axis, and for nz and the z-axis
                    [default: nx=1 ny=5 nz=0]
     -verb       = print progress reports (this is a slow program)
    
    NOTES
    *Systematically moves the anat dataset around and find the shift
      that maximizes overlap between the anat dataset and the EPI
      dataset.  No rotations are done.
    *Note that if you use -prefix, a new dataset will be created that
      is a copy of the anat, except that it's origin will be shifted
      and it will have a different ID code than the anat.  If you want
      to use this new dataset as the anatomy parent for the EPI
      datasets, you'll have to use
        3drefit -apar ppp+orig eee1+orig eee2+orig ...
    *If no new dataset is written (no -prefix option), then you
      can use the 3drefit command emitted at the end to modify
      the origin of the anat dataset.  (Assuming you trust the
      results - visual inspection is recommended!)
    *The reason the default search grid is mostly along the EPI y-axis
      is that axis is usually the phase-encoding direction, which is
      most subject to displacement due to off-resonance effects.
    *Note that the time this program takes will be proportional to
      (2*nx+1)*(2*ny+1)*(2*nz+1), so using a very large search grid
      will result in a very large usage of CPU time.
    *Recommended usage:
     + Make a 1-brick function volume from a typical EPI dataset:
         3dbucket -fbuc -prefix epi_fb epi+orig
     + Use 3dIntracranial to scalp a T1-weighted volume:
         3dIntracranial -anat spgr+orig -prefix spgr_st
     + Use 3dAnatNudge to produce a shifted anat dataset
         3dAnatNudge -anat spgr_st+orig -epi epi_fb+orig -prefix spgr_nudge
     + Start AFNI and look at epi_fb overlaid in color on the
        anat datasets spgr_st+orig and spgr_nudge+orig, to see if the
        nudged dataset seems like a better fit.
     + Delete the nudged dataset spgr_nudge.
     + If the nudged dataset DOES look better, then apply the
        3drefit command output by 3dAnatNudge to spgr+orig.
    *Note that the x-, y-, and z-axes for the epi and anat datasets
      may point in different directions (e.g., axial SPGR and
      coronal EPI).  The 3drefit command applies to the anat
      dataset, NOT to the EPI dataset.
    *If the program runs successfully, the only thing set to stdout
      will be the 3drefit command string; all other messages go to
      stderr.  This can be useful if you want to capture the command
      to a shell variable and then execute it, as in the following
      csh fragment:
         set cvar = `3dAnatNudge ...`
         if( $cvar[1] == "3drefit" ) $cvar
      The test on the first sub-string in cvar allows for the
      possibility that the program fails, or that the optimal
      nudge is zero.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
