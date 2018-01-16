*************
@ROI_Corr_Mat
*************

.. _@ROI_Corr_Mat:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Script to produce an NxN ROI correlation matrix of N ROIs.
    
    Usage: 
    @ROI_Corr_Mat    <-ts TimeSeriesVol>  
                     <-roi ROIVol>  
                     <-prefix output>
                     [<-roisel ROISEL>]
                     [-zval]
                     [-mat FULL, TRI, TRI_ND]
                     [-verb] [-dirty]
    
    Parameters
       -ts TimeSeriesVol: Time series volume
       -roi ROIVol: ROI volume
                 This script will resample the ROI volume to match the 
                 resolution of the EPI if the number of voxels in each of
                 the three directions is not the same. 
                 ROIs are resampled using NN interpolation. If you'd
                 rather interpolate the epi, then do so before you run 
                 this script.
       -prefix output: Use output for a prefix
       -roisel ROISEL: Force processing of ROI label (integers) listed
                       in ROISEL 1D file. The default is to process all
                       ROIs in ROIvol.
                       It is important to use this option when processing
                       data accross subjects with differing ROIVol for 
                       input. If all ROIVol volumes do not have the same
                       set of ROI labels then the correlation matrices 
                       would be of differing sizes.
                       See 3dRank for obtaining a list of ROI labels in 
                       a volume.
              NOTE: ROI labels in ROISEL that do not exist in ROIvol will
                    be replaced with empty vectors.
    
       -zval: Output a zscore version of the correlation matrix.
       -mat OPT: Output matrix in different manners depending on OPT:
                  FULL  --> Full matrix 
                  TRI   --> Triangular
                  TRI_ND--> Triangular, without diagonal (default)
    
       -dirty: Keep temporary files
       -keep_tmp: Keep temporary files
       -echo: set echo (echo all commands to screen)
       -verb: Verbose flag
    
    Example:
    @ROI_Corr_Mat     -ts s620_rest_r1+orig \
                      -roi SUMA/aparc.a2005s+aseg.nii \
                      -prefix s620_matrix_all_ROIs
    
    
    How to read correlation matrix:
    The correlation matrix is created in .1D and .BRIK formats
    
    1. Choose undelay master.2droi.row+orig
          and overlay s620_matrix_my_ROIs_Zval+orig
    2. Push Define Datamode Button -> Misc Button -> Voxel Coords
    3. Click axial button, and turn + LR Mirror off.
    (i, j) on afni GUI means that the selected pixel is 
    r- or Z-values presenting correlation between i-th ROI and j-th ROI.
    
    Written by Hang Joon Jo, Modified by Ziad S. Saad.  (05/11/2009)
