*************
3dNwarpAdjust
*************

.. _ahelp_3dNwarpAdjust:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dNwarpAdjust [options]
    
    This program takes as input a bunch of 3D warps, averages them,
    and computes the inverse of this average warp.  It then composes
    each input warp with this inverse average to 'adjust' the set of
    warps.  Optionally, it can also read in a set of 1-brick datasets
    corresponding to the input warps, and warp each of them, and average
    those.
    
               Input warps: Wi(x) for i=1..N
              Average warp: Wbar(x) = mean of the displacements in Wi(x)
           Inverse average: Wbin(x) = inverse of Wbar(x)
            Adjusted warps: Ai(x) = Wi(Wbin(x))
    
           Source datasets: Di(x) for i=1..N
       Output mean dataset: average of Di(Ai(x))
    
    The logic behind this arcane necromancy is the following sophistry:
    
       We use 3dQwarp to warp each Di(x) to match a template T(x), giving
       warp Wi(x) such that Di(Wi(x)) matches T(x).  Now we want to average
       these warped Di datasets to create a new template; say
         B(x) = average of Di(Wi(x))
       But the warps might be biased (e.g., have net shrinkage of the volumes).
       So we compute the average warp Wbar(x), and its inverse Wbin(x), and then
       instead we want to use as the new template B(Wbin(x)), which will 'put back'
       each x to a bias-corrected location.  So then we have
         B(Wbin(x)) = average of Di(Wi(Wbin(x)))
       which is where the 'adjusted warp' Ai(x) = Wi(Wbin(x)) comes from.
    
    All these calculcations could be done with other programs and a script,
    but the goal of this program is to make them faster and simpler to combine.
    It is intended to be used in an incremental template-building script, and
    probably has no other utility (cf. the script @toMNI_Qwarpar).
    
    OPTIONS:
    --------
     -nwarp  w1 w2 ... = List of input 3D warp datasets (at least 5).
                         The list ends when a command line argument starts
                         with a '-' or the command line itself ends.
                         * This 'option' is REQUIRED!
                    -->>** Each input warp is adjusted, and the altered warp
                           over-writes the input dataset. (Therefore, there is
                           no reason to run 3dNwarpAdjust twice over the same
                           collection of warp datasets!)
                         * These input warps do not have to be defined on
                           exactly the same grids, but the grids must be
                           'conformant' -- that is, they have to have the
                           the same orientation and grid spacings.  Warps
                           will be extended to match the minimum containing
                           3D rectangular grid, as needed.
    
     -source d1 d2 ... = List of input 3D datasets to be warped by the adjusted
                         warp datasets.  There must be exactly as many of these
                         datasets as there are input warps.
                         * This option is NOT required.
                         * These datasets will NOT be altered by this program.
                         * These datasets DO have to be on the same 3D grid
                           (so they can be averaged after warping).
    
     -prefix ppp       = Use 'ppp' for the prefix of the output mean dataset.
                         (Only needed if the '-source' option is also given.)
                         The output dataset will be on the common grid shared
                         by the source datasets.
    
     =========================================================================
    * This binary version of 3dNwarpAdjust is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
     =========================================================================
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
