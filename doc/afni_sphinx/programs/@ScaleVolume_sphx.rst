************
@ScaleVolume
************

.. _@ScaleVolume:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: @ScaleVolume <-input DSET> <-prefix PREFIX>
                         [-perc_clip P0 P1] [-val_clip V0 V1]
                         [-scale_by_mean] [-scale_by_median]
                         [-norm] [-mask MSET]
    
    -input DSET: Dset to scale
    -prefix PREFIX: Prefix of output
    -mask MSET: Restrict to non-zero values of MSET
    Method 1: (default)
    Scale a volume so that its values range between V0 and V1
    -val_clip V0 V1: Min and Max of output dset
                     Default V0 = 0 and V1 = 255
    -perc_clip P0 P1: Set lowest P0 percentile to Min 
                      and highest P1 percentile to Max
                      Default P0 = 2 and P1 = 98
    Output sub-brick labels are prefixed with SV.
    
    At the moment, Method 1 only operates on volumes with one sub-brick
    
    Method 2:
    -scale_by_mean: Divide each sub-brick by mean of non-zero voxels
    Output sub-brick labels are prefixed with mea.
    -scale_by_median: Divide each sub-brick by median of non-zero voxels
    Output sub-brick labels are prefixed with med.
    
    Method 3:
    -norm: For each time series T, Tnorm= (T-mean(T))/stdev(T)
    Output sub-brick labels are prefixed with tz.
    
    Method 4:
    -feat_norm: For each sub-brick B, Bnorm= (B-min(B))/(max(B)-min(B))*99+1
    Output sub-brick labels are prefixed with r.
    
    Method 5:
    -feat_znorm: For each sub-brick B, Bnorm= (B-mean(B))/stdev(B)
    Output sub-brick labels are prefixed with z.
    
    Global Help Options:
    --------------------
    
       -h_web: Open webpage with help for this program
       -hweb: Same as -h_web
       -h_view: Open -help output in a GUI editor
       -hview: Same as -hview
       -all_opts: List all of the options for this script
       -h_find WORD: Search for lines containing WORD in -help
