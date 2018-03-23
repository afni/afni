.. _ahelp_3dkmeans:

********
3dkmeans
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    3d+t Clustering segmentation, command-line version.
        Based on The C clustering library.
        Copyright (C) 2002 Michiel Jan Laurens de Hoon.
    USAGE: 3dkmeans [options]
    options:
      -v, --version Version information
      -f filename: Input data to be clustered.   
                    You can specify multiple filenames in sequence
                    and they will be catenated internally.
             e.g: -f F1+orig F2+orig F3+orig ...
               or -f F1+orig -f F2+orig -f F3+orig ...
      -input filename: Same as -f
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
    
      -u jobname    Allows you to specify a different name for the 
                    output files.
                    (default is derived from the input file name)
      -prefix PREFIX Allows you to specify a prefix for the output 
                     volumes. Default is the same as jobname
                     There are two output volumes, one for the cluster
                     membership and one with distance measures.
                     The distance dataset, mostly for debugging puposes
                     is formatted as follows:
                     Sub-brick 0: Dc = 100*(1-Ci)+100*Di/(Dmax)
                     with Ci the cluster number for voxel i, Di the 
                     distance of voxel i to the centroid of its 
                     assigned cluster, Dmax is the maximum distance in
                     cluster Ci.
                     Sub-bricks 1..k: Dc0k contains the distance of a
                     voxel's data to the centroid of cluster k.
                     Sub-brick k+1: Dc_norm = (1.0-Di/Ei)*100.0, where 
                     Ei is the smallest distance of voxel i to 
                     the remaining clusters that is larger than Di.
      -g [0..8]     Specifies distance measure for clustering
                    Note: Weight is a vector as long as the signatures
                    and used when computing distances. However for the
                    moment, all weights are set to 1
                    0: No clustering
                    1: Uncentered correlation distance
                        Same as Pearson distance, except
                        the means of v and s are not removed
                        when computing correlation.
                    2: Pearson distance
                        = (1-Weighted_Pearson_Correlation(v,s))
                    3: Uncentered correlation distance, absolute value
                        Same as abs(Pearson distance), except
                        the means of v and s are not removed
                        when computing correlation.
                    4: Pearson distance, absolute value
                        = (1-abs(Weighted_Pearson_Correlation(v,s)))
                    5: Spearman's rank distance
                        = (1-Spearman_Rank_Correlation(v,s))
                       No weighting is used
                    6: Kendall's distance
                        = (1-Kendall_Tau(v,s))
                       No weighting is used
                    7: Euclidean distance between v and s
                        = 1/sum(weight) * sum(weight[i]*(v[i]-s[i])^2)
                    8: City-block distance
                        = 1/sum(weight) * sum(weight[i]*abs(v[i]-s[i]))
    
           (default for -g is 1, 7 if input has one value per voxel)
    
      -k number     Specify number of clusters
      -remap  METH  Reassign clusters numbers based on METH:
                       NONE: No remapping (default)
                       COUNT: based on cluster size ascending
                      iCOUNT: COUNT, descending
                       MAG:  based on ascending magnitude of centroid
                      iMAG: MAG, descending
      -labeltable LTFILE: Attach labeltable LTFILE to clustering
                          output. This labeltable will overwrite
                          a table that is taken from CLUST_INIT
                          should you use -clust_init option.
      -clabels LAB1 LAB2 ...: Provide a label for each cluster.
                              Labels cannot start with '-'.
      -clust_init CLUST_INIT: Specify a dataset to initialize 
                              clustering. This option sets -r 0 .
                              If CLUST_INIT has a labeltable and 
                              you do not specify one then CLUST_INIT's
                              table is used for the output
      -r number     For k-means clustering, the number of times the
                    k-means clustering algorithm is run
                    (default: 0 with -clust_init, 1 otherwise)
      -rsigs SIGS   Calculate distances from each voxel's signature
                    to the signatures in SIGS. 
                    SIGS is a multi-column 1D file with each column
                    being a signature.
                    The output is a dset the same size as the input
                    with as many sub-bricks as there are columns in 
                    SIGS.
                    With this option, no clustering is done.
      -verb         verbose 
      -write_dists  Output text files containing various measures.
                    FILE.kgg.1D : Cluster assignments 
                    FILE.dis.1D : Distance between clusters
                    FILE.cen.1D : Cluster centroids
                    FILE.info1.1D: Within cluster sum of distances
                    FILE.info2.1D: Maximum distance within each cluster
                    FILE.vcd.1D: Distance from voxel to its centroid
      -voxdbg I J K Output debugging info for voxel I J K
      -seed SEED    Seed for the random number generator.
