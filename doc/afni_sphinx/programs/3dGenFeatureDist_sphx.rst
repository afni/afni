****************
3dGenFeatureDist
****************

.. _3dGenFeatureDist:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    3dGenFeatureDist produces hives.
       -classes 'CLASS_STRING': CLASS_STRING is a semicolon delimited
                             string of class labels. For example
                             -classes 'CSF; WM; GM'
    
       -OTHER: Add histograms for an 'OTHER' class that has a uniform pdf.
    
       -no_OTHER: Opposite of -OTHER.
    
       -features 'FEATURES_STRING': FEATURES_STRING is a semicolon delimited
                             string of features. For example
                             -features 'MEAN.00_mm; median.19_mm; ...'
    
       -sig 'FEATURE_VOL1 FEATURE_VOL2 ...': Specify volumes that define
                             the features. Each sub-brick is a feature
                             and the sub-brick's name is used to name the 
                             feature. Multiple volumes get catenated.
                             Each occurence of -sig option must be paired with
                             a -samp option. Think of each pair of '-sig, -samp'
                             options as describing data on the same voxel grid; 
                             Think from the same subject. When specifying 
                             training data from K subjects, you will end up using
                             K pairs of '-sig, -samp'.
                             All volumes from the kth -sig instance should have 
                             the same voxel grid as each other and as that of
                             the kth -samp datasets.
    
       -samp 'SAMPLE_VOX1 SAMPLE_VOX2 ...': Specify which voxels belong to
                             each class of interest. Each of the volumes
                             should contain voxel values (keys) that are
                             defined in -labeltable. You can specify multiple
                             volumes, they all get catenated. Any volume can
                             contain voxels from 1 or more classes.
                             Each occurence of -samp option must be paired with
                             a -sig option. Think of each pair of '-sig, -samp'
                             options as describing data on the same voxel grid; 
                             Think from the same subject. When specifying 
                             training data from K subjects, you will end up using
                             K pairs of '-sig, -samp'.
                             All volumes from the kth -samp instance should have 
                             the same voxel grid as each other and as that of
                             the kth -sig datasets.
    
       -hspec FEATURE MIN MAX NBINS: Set histogram parameters for feature FEATURE
                                  FEATURE: String label of feature
                                  MIN, MAX: Range of histogram
                                  NBINS: Number of bins
            Use this option to set the histogram parameters for the features for
            the automatic parameter selection was lousy. You can specify 
            for multiple features by using multiple -hspec instances. The only
            condition is that all feature labels (FEATURE) must be part of the 
            set named in -features.
    
       -prefix PREF: PREF is the prefix for all output volume that are not 
                  debugging related.
    
         default: GenFeatDist
       -ShowTheseHists HISTNAMES: Show histograms specified by HISTNAMES and quit.
                  HISTNAMES can specify just one .niml.hist file or a bunch of 
                  them using a space, or comma separated list. 
                  List multiple names between quotes.
    
       -overwrite: An option common to almost all AFNI programs. It is 
                automatically turned on if you provide no PREF.
    
       -debug: Debugging level
    
         default: 1
       -labeltable LT: Specify the label table
    
