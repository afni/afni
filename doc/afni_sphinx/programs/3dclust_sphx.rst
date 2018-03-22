*******
3dclust
*******

.. _ahelp_3dclust:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    
    Program: 3dclust 
    Author:  RW Cox et alii 
    Date:    12 Jul 2017 
    
    3dclust - performs simple-minded cluster detection in 3D datasets       
                                                                            
         This program can be used to find clusters of 'active' voxels and   
         print out a report about them.                                     
          * 'Active' refers to nonzero voxels that survive the threshold    
             that you (the user) have specified                             
          * Clusters are defined by a connectivity radius parameter 'rmm'   
            *OR*
            Clusters are defined by how close neighboring voxels must
            be in the 3D grid:
              first nearest neighbors  (-NN1)
              second nearest neighbors (-NN2)
              third nearest neighbors  (-NN3)
                                                                            
          Note: by default, this program clusters on the absolute values    
                of the voxels                                               
    ----------------------------------------------------------------------- 
    Usage:
                                                                            
       3dclust [editing options] [other options] rmm vmul dset ...          
                                                                            
     *OR*
                                                                            
       3dclust [editing options] -NNx dset ...
         where '-NNx' is one of '-NN1' or '-NN2' or '-NN3':
          -NN1 == 1st nearest-neighbor (faces touching) clustering
          -NN2 == 2nd nearest-neighbor (edges touching) clustering
          -NN2 == 3rd nearest-neighbor (corners touching) clustering
         Optionally, you can put an integer after the '-NNx' option, to
         indicate the minimum number of voxels to allow in a cluster;
         for example: -NN2 60
    ----------------------------------------------------------------------- 
    Examples:                                                               
    ---------                                                               
                                                                            
        3dclust         -1clip   0.3  5 2000 func+orig'[1]'                 
        3dclust -1noneg -1thresh 0.3  5 2000 func+orig'[1]'                 
        3dclust -1noneg -1thresh 0.3  5 2000 func+orig'[1]' func+orig'[3]   
                                                                            
        3dclust -noabs  -1clip 0.5   -dxyz=1  1  10 func+orig'[1]'          
        3dclust -noabs  -1clip 0.5            5 700 func+orig'[1]'          
                                                                            
        3dclust -noabs  -2clip 0 999 -dxyz=1 1  10 func+orig'[1]'           
                                                                            
        3dclust                   -1clip 0.3  5 3000 func+orig'[1]'         
        3dclust -quiet            -1clip 0.3  5 3000 func+orig'[1]'         
        3dclust -summarize -quiet -1clip 0.3  5 3000 func+orig'[1]'         
        3dclust -1Dformat         -1clip 0.3  5 3000 func+orig'[1]' > out.1D
    ----------------------------------------------------------------------- 
                                                                            
    Arguments (must be included on command line):                           
    ---------                                                               
                                                                            
    THE OLD WAY TO SPECIFY THE TYPE OF CLUSTERING
    
       rmm            : cluster connection radius (in millimeters).         
                        All nonzero voxels closer than rmm millimeters      
                        (center-to-center distance) to the given voxel are  
                        included in the cluster.                            
                         * If rmm = 0, then clusters are defined by nearest-
                           neighbor connectivity                            
                                                                            
       vmul           : minimum cluster volume (micro-liters)               
                        i.e., determines the size of the volume cluster.    
                         * If vmul = 0, then all clusters are kept.         
                         * If vmul < 0, then the absolute vmul is the minimum
                              number of voxels allowed in a cluster.        
    
      If you do not use one of the '-NNx' options, you must give the
      numbers for rmm and vmul just before the input dataset name(s)
    
    THE NEW WAY TO SPECIFY TYPE OF CLUSTERING [13 Jul 2017]
    
       -NN1 or -NN2 or -NN3
    
      If you use one of these '-NNx' options, you do NOT give the rmm
      and vmul values.  Instead, after all the options that start with '-',
      you just give the input dataset name(s).
      If you want to set a minimum cluster size using '-NNx', put the minimum
      voxel count immediately after, as in '-NN3 100'.
    
    FOLLOWED BY ONE (or more) DATASETS
                                                                            
       dset           : input dataset (more than one allowed, but only the  
                        first sub-brick of the dataset)                     
                                                                            
     The results are sent to standard output (i.e., the screen):            
     if you want to save them in a file, then use redirection, as in
    
       3dclust -1thresh 0.4 -NN2 Elvis.nii'[1]' > Elvis.clust.txt
                                                                            
    ----------------------------------------------------------------------- 
                                                                            
    Options:                                                                
    -------                                                                 
                                                                            
    * Editing options are as in 3dmerge (see 3dmerge -help)                 
      (including -1thresh, -1dindex, -1tindex, -dxyz=1 options)             
    
    * -NN1        => described earlier;
      -NN2        => replaces the use of 'rmm' to specify the
      -NN3        => clustering method (vmul is set to 2 voxels)
                                                                            
    * -noabs      => Use the signed voxel intensities (not the absolute     
                     value) for calculation of the mean and Standard        
                     Error of the Mean (SEM)                                
                                                                            
    * -summarize  => Write out only the total nonzero voxel                 
                     count and volume for each dataset                      
                                                                            
    * -nosum      => Suppress printout of the totals                        
                                                                            
    * -verb       => Print out a progress report (to stderr)                
                     as the computations proceed                            
                                                                            
    * -1Dformat   => Write output in 1D format (now default). You can       
                     redirect the output to a .1D file and use the file     
                     as input to whereami for obtaining Atlas-based         
                     information on cluster locations.                      
                     See whereami -help for more info.                      
    * -no_1Dformat=> Do not write output in 1D format.                      
                                                                            
    * -quiet      => Suppress all non-essential output                      
                                                                            
    * -mni        => If the input dataset has the +tlrc view, this option   
                     will transform the output xyz-coordinates from TLRC to 
                     MNI space.
                                                                            
               N.B.0: Only use this option if the dataset is in Talairach   
                      space, NOT when it is already in MNI space.           
               N.B.1: The MNI template brain is about 5 mm higher (in S),   
                      10 mm lower (in I), 5 mm longer (in PA), and tilted   
                      about 3 degrees backwards, relative to the Talairach- 
                      Tournoux Atlas brain.  For more details, see          
                        http://www.mrc-cbu.cam.ac.uk/Imaging/mnispace.html  
               N.B.2: If the input dataset does not have the +tlrc view,    
                      then the only effect is to flip the output coordinates
                      to the 'LPI' (neuroscience) orientation, as if you    
                      gave the '-orient LPI' option.)                       
                                                                            
    * -isovalue   => Clusters will be formed only from contiguous (in the   
                     rmm sense) voxels that also have the same value.       
                                                                            
               N.B.:  The normal method is to cluster all contiguous        
                      nonzero voxels together.                              
                                                                            
    * -isomerge   => Clusters will be formed from each distinct value       
                     in the dataset; spatial contiguity will not be         
                     used (but you still have to supply rmm and vmul        
                     on the command line).                                  
                                                                            
               N.B.:  'Clusters' formed this way may well have components   
                       that are widely separated!                           
    
    * -inmask  =>    If 3dClustSim put an internal attribute into the       
                     input dataset that describes a mask, 3dclust will      
                     use this mask to eliminate voxels before clustering,   
                     if you give this option.  '-inmask' is how the AFNI    
                     AFNI Clusterize GUI works by default.                  
                       [If there is no internal mask in the dataset]        
                       [header, then '-inmask' doesn't do anything.]        
    
               N.B.: The usual way for 3dClustSim to have put this internal 
                     mask into a functional dataset is via afni_proc.py.    
                                                                            
    * -prefix ppp => Write a new dataset that is a copy of the              
                     input, but with all voxels not in a cluster            
                     set to zero; the new dataset's prefix is 'ppp'         
                                                                            
               N.B.:  Use of the -prefix option only affects the            
                      first input dataset.                                  
    
    * -savemask q => Write a new dataset that is an ordered mask, such      
                     that the largest cluster is labeled '1', the next      
                     largest '2' and so forth.  Should be the same as       
                     '3dmerge -1clust_order' or Clusterize 'SaveMsk'.       
      -binary     => This turns the output of '-savemask' into a binary     
                     (0 or 1) mask, rather than a cluster-index mask.       
              **-->> If no clusters are found, the mask is not written!     
    
    ----------------------------------------------------------------------- 
     N.B.: 'N.B.' is short for 'Nota Bene', Latin for 'Note Well';          
           also see http://en.wikipedia.org/wiki/Nota_bene                  
    ----------------------------------------------------------------------- 
                                                                            
    E.g., 3dclust -1clip 0.3  5  3000 func+orig'[1]'                        
                                                                            
      The above command tells 3dclust to find potential cluster volumes for 
      dataset func+orig, sub-brick #1, where the threshold has been set     
      to 0.3 (i.e., ignore voxels with an activation threshold of >0.3 or   
      <-0.3.  Voxels must be no more than 5 mm apart, and the cluster volume
      must be at least 3000 micro-liters in size.                           
                                                                            
    Explanation of 3dclust Output:                                          
    -----------------------------                                           
                                                                            
       Volume       : Volume that makes up the cluster, in microliters (mm^3)
                      (or the number of voxels, if -dxyz=1 is given)        
                                                                            
       CM RL        : Center of mass (CM) for the cluster in the Right-Left 
                      direction (i.e., the coordinates for the CM)          
                                                                            
       CM AP        : Center of mass for the cluster in the                 
                      Anterior-Posterior direction                          
                                                                            
       CM IS        : Center of mass for the cluster in the                 
                      Inferior-Superior direction                           
                                                                            
       minRL, maxRL : Bounding box for the cluster, min and max             
                      coordinates in the Right-Left direction               
                                                                            
       minAP, maxAP : Min and max coordinates in the Anterior-Posterior     
                      direction of the volume cluster                       
                                                                            
       minIS, max IS: Min and max coordinates in the Inferior-Superior      
                      direction of the volume cluster                       
                                                                            
       Mean         : Mean value for the volume cluster                     
                                                                            
       SEM          : Standard Error of the Mean for the volume cluster     
                                                                            
       Max Int      : Maximum Intensity value for the volume cluster        
                                                                            
       MI RL        : Coordinate of the Maximum Intensity value in the      
                      Right-Left direction of the volume cluster            
                                                                            
       MI AP        : Coordinate of the Maximum Intensity value in the      
                      Anterior-Posterior direction of the volume cluster    
                                                                            
       MI IS        : Coordinate of the Maximum Intensity value in the      
                      Inferior-Superior direction of the volume cluster     
    ----------------------------------------------------------------------- 
                                                                            
    Nota Bene:                                                              
                                                                            
       * The program does not work on complex- or rgb-valued datasets!      
                                                                            
       * Using the -1noneg option is strongly recommended!                  
                                                                            
       * 3D+time datasets are allowed, but only if you use the              
         -1tindex and -1dindex options.                                     
                                                                            
       * Bucket datasets are allowed, but you will almost certainly         
         want to use the -1tindex and -1dindex options with these.          
                                                                            
       * SEM values are not realistic for interpolated data sets!           
         A ROUGH correction is to multiply the SEM of the interpolated      
         data set by the square root of the number of interpolated          
         voxels per original voxel.                                         
                                                                            
       * If you use -dxyz=1, then rmm should be given in terms of           
         voxel edges (not mm) and vmul should be given in terms of          
         voxel counts (not microliters).  Thus, to connect to only          
         3D nearest neighbors and keep clusters of 10 voxels or more,       
         use something like '3dclust -dxyz=1 1.01 10 dset+orig'.            
         In the report, 'Volume' will be voxel count, but the rest of       
         the coordinate dependent information will be in actual xyz         
         millimeters.                                                       
                                                                            
      * The default coordinate output order is DICOM.  If you prefer        
        the SPM coordinate order, use the option '-orient LPI' or           
        set the environment variable AFNI_ORIENT to 'LPI'.  For more        
        information, see file README.environment.                           
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
