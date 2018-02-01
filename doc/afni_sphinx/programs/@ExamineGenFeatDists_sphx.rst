********************
@ExamineGenFeatDists
********************

.. _@ExamineGenFeatDists:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @ExamineGenFeatDists <-fdir FEATURES_DIR> 
     
    Examine histograms produced by 3dGenFeatDists
    
    -fdir DIR: output directory of 3dGenFeatDists
    -fwild WILD1 [WILD2 ...]: Wildcards used to select feature histograms
                              under DIR.
                              Histograms picked would be those named:
                              h.*WILD1.niml.hist and h.*WILD1-G-*.niml.hist
    -suffix SUFF: Output suffix, added to output images. Default nosuff
    -exfeat FEAT1 [FEAT2 ...]: Exclude following features. String matching
                                is partial
    -exclass CLSS1 [CLSS2 ...]: Exclude following classes. String matching 
                                 is partial
    -odir DIR: Output directory, default is DIR
    -nx NX: Set number of panel along the horizontal direction
    -echo: Set echo
    -help: this message
    
     See also @FeatureHists 
    
    Example:
    @ExamineGenFeatDists    -fwild sc9 Xz Yz Zz FA.MAD07 MD \
                            -fdir GenFeatDist.sc9 \
                            -exfeat mean z.FA. z.MD \
                            -exclass air \
                            -odir GenFeatDist.sc9
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
