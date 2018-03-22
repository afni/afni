********
AlphaSim
********

.. _AlphaSim:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs alpha probability simulations; among other
    things, it computes the probability of a random field of noise
    producing a cluster of a given size after the noise is thresholded
    at a given level ('-pthr').
    
     *** PLEASE do not use this program any more.  Use 3dClustSim! ***
    
    Usage: 
    AlphaSim 
    -nx n1        n1 = number of voxels along x-axis                      
    -ny n2        n2 = number of voxels along y-axis                      
    -nz n3        n3 = number of voxels along z-axis                      
    -dx d1        d1 = voxel size (mm) along x-axis                       
    -dy d2        d2 = voxel size (mm) along y-axis                       
    -dz d3        d3 = voxel size (mm) along z-axis                       
    -nxyz n1 n2 n3   = give all 3 grid dimensions at once                 
    -dxyz d1 d2 d3   = give all 3 voxel sizes at once                     
    [-mask mset]      Use the 0 sub-brick of dataset 'mset' as a mask     
                        to indicate which voxels to analyze (a sub-brick  
                        selector is allowed)  [default = use all voxels]  
                      Note:  The -mask command also REPLACES the          
                             -nx, -ny, -nz, -dx, -dy, and -dz commands,   
                             and takes the volume dimensions from 'mset'. 
    [-fwhm s]     s  = Gaussian filter width (FWHM, in mm)                
    [-fwhmx sx]   sx = Gaussian filter width, x-axis (FWHM)               
    [-fwhmy sy]   sy = Gaussian filter width, y-axis (FWHM)               
    [-fwhmz sz]   sz = Gaussian filter width, z-axis (FWHM)               
    [-sigma s]    s  = Gaussian filter width (1 sigma, in mm)             
    [-sigmax sx]  sx = Gaussian filter width, x-axis (1 sigma)            
    [-sigmay sy]  sy = Gaussian filter width, y-axis (1 sigma)            
    [-sigmaz sz]  sz = Gaussian filter width, z-axis (1 sigma)            
    
    [-power]      perform statistical power calculations                  
    [-ax n1]      n1 = extent of active region (in voxels) along x-axis   
    [-ay n2]      n2 = extent of active region (in voxels) along y-axis   
    [-az n3]      n3 = extent of active region (in voxels) along z-axis   
    [-zsep z]     z = z-score separation between signal and noise         
    
    [-rmm r]      r  = cluster connection radius (mm)                     
                       Default is nearest neighbor connection only.       
    -pthr p       p  = individual voxel threshold probability             
    -iter n       n  = number of Monte Carlo simulations                  
    [-quiet]      suppress lengthy per-iteration screen output            
    [-out file]   file = name of output file [default value = screen]     
    [-max_clust_size size]  size = maximum allowed voxels in a cluster    
    [-seed S]     S  = random number seed
                       default seed = 123456789
                       if seed=0, then program will randomize it
    [-fast]       Use a faster random number generator:
                    Can speed program up by about a factor of 2,
                    but detailed results will differ slightly since
                    a different sequence of random values will be used.
    [-approx]     Compute an analytic approximation to the Alpha(i)
                    result for cluster size i, and print a column of that
                    value in the output (only if '-power' is NOT used)
                ** This analytic approximation is a way to extrapolate
                    the alpha value for cluster sizes beyond the
                    reaches of the simulation. The formula for it is
                    printed above the output table; see the example below.
                ** The analytic approximation is only computed if the
                    table of cluster size vs. alpha is 'large enough'.
                ** The approximation formula is of 'extreme value' type,
                    possibly with an adjustment for smaller i and larger Alpha.
    
    Unix environment variables you can use:
    ---------------------------------------
     Set AFNI_BLUR_FFT to YES to require blurring be done with FFTs
       (the oldest way, and slowest).
     Set AFNI_BLUR_FFT to NO and AFNI_BLUR_FIROLD to YES to require
       blurring to be done with the old (crude) FIR code (not advised).
     If neither of these are set, then blurring is done using the newer
       (more accurate) FIR code (recommended).
     Results will differ in detail depending on the blurring method
       used to generate the simulated noise fields.
    
    SAMPLE OUTPUT:
    --------------
     AlphaSim -nxyz 64 64 20 -dxyz 3 3 3 -iter 10000 -pthr 0.004 -fwhm 5 \
              -quiet -fast -approx
    # Alpha(i) approx 1-exp[-exp(8.720-2.2166*i^0.58-0.05743*posval(12-i)^1.0)]
    # Cl Size   Frequency    CumuProp     p/Voxel   Max Freq       Alpha    Approx
          1       1024002    0.584689  0.00414373          0    1.000000  1.000000
          2        358143    0.789183  0.00289373          0    1.000000  1.000000
          3        156346    0.878455  0.00201936          0    1.000000  1.000000
          4         87554    0.928447  0.00144680          0    1.000000  1.000000
          5         48445    0.956108  0.00101929          6    1.000000  1.000000
          6         29126    0.972738  0.00072361         81    0.999400  0.999736
          7         17743    0.982869  0.00051028        407    0.991300  0.992216
          8         11220    0.989276  0.00035867       1082    0.950600  0.948274
          9          6722    0.993114  0.00024910       1453    0.842400  0.844084
         10          4251    0.995541  0.00017525       1564    0.697100  0.697100
         11          2708    0.997087  0.00012336       1426    0.540700  0.543212
         12          1736    0.998079  0.00008700       1132    0.398100  0.407466
         13          1164    0.998743  0.00006157        875    0.284900  0.284900
         14           744    0.999168  0.00004309        615    0.197400  0.195818
         15           485    0.999445  0.00003038        434    0.135900  0.133634
         16           324    0.999630  0.00002150        302    0.092500  0.091099
         17           213    0.999752  0.00001517        196    0.062300  0.062256
         18           140    0.999832  0.00001075        136    0.042700  0.042736
         19            87    0.999881  0.00000767         84    0.029100  0.029499
         20            62    0.999917  0.00000566         61    0.020700  0.020485
         21            49    0.999945  0.00000414         49    0.014600  0.014314
         22            31    0.999962  0.00000289         31    0.009700  0.010064
         23            16    0.999971  0.00000205         16    0.006600  0.007119
         24            10    0.999977  0.00000161         10    0.005000  0.005065
         25            11    0.999983  0.00000131         11    0.004000  0.003624
         26            12    0.999990  0.00000098         12    0.002900  0.002607
         27             3    0.999992  0.00000060          3    0.001700  0.001885
         28             4    0.999994  0.00000050          4    0.001400  0.001370
         29             7    0.999998  0.00000036          7    0.001000  0.001000
         30             1    0.999999  0.00000011          1    0.000300  0.000733
         31             2    1.000000  0.00000008          2    0.000200  0.000540
    
     That is, thresholded random noise alone (no signal) would produce a cluster
     of size 18 or larger about 4.27% (Alpha) of the time, in a 64x64x20 volume
     with cubical 3 mm voxels and a FHWM noise smoothness of 5 mm, and an uncorrected
     uncorrected (per voxel) p-value of 0.004 -- this combination of voxel-wise and
     cluster-size thresholds would be a logical one to use for a functional map that
     had these parameters.
    
     If you run the exact command above, you will get slightly different results,
     due to variations in the random numbers generated in the simulations.
    
     To plot the approximation on top of the empirical alpha, if the above file
     is stored as alp.1D, then the following command can be used:
       1dplot -start 1 -one -ytran 'log(-log(1-a))' alp.1D'[5,6]'
     These will plot the log(log) transformed Alpha(i) and the log(log)
     transformed approximation together, so you can see how they fit,
     especially for the large i and small Alpha cases.  Another comparison
     technique is to plot the ratio of Approx(i) to Alpha(i):
       1deval -a alp.1D'[5]' -b alp.1D'[6]' -expr 'b/a' | 1dplot -start 1 -stdin
     (Since Alpha(i) is always > 0 in the table, there is no division by zero.)
    
     The analytic approximation formula above uses the function 'posval(x)',
     which is defined to be 'max(x,0)' -- this is the correction for small i
     (in this example, i < 12).  The syntax is compatible with 1deval and 3dcalc.
     The breakpoint for the small i/large Alpha correction is set to be at the
     cluster size i where Alpha(i) is about 0.3 [in the sample above, 'posval(12-i)'].
     For larger i/smaller Alpha, the approximation is of the simple form
       Alpha(i) = 1-exp[-exp(a-b*i^p)]
     where a, b, p are constants. For a pure extreme value distribution, p=1;
     I've found that allowing p < 1 gives slightly better fits in some cases.
    
    
     *** PLEASE do not use this program any more.  Use 3dClustSim! ***
    
     =========================================================================
    * This binary version of AlphaSim is compiled using OpenMP, a semi-
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
    * OpenMP compilation implies '-fast'
