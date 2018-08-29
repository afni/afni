.. _fp_postpre_IV:

Post-preproc, IV: Tracking
======================================

.. contents:: :local:

.. highlight:: Tcsh

Overview
--------

**under construction**

Probabilistic tracking and **3dTrackID**
--------------------------------

**under construction**


**Proc:** This is an example of fully probabilistic tracking through
the set of "inflated" GM regions from the previous ``3dROIMaker``
step.  Of the two output FS aseg+aparc atlases, this uses the "2000"
one::

    # for I/O, same path variable as above
    set path_P_ss = data_proc/SUBJ_001

    # for variable simplicity, just go to the dir with the ROI maps
    cd $path_P_ss/dwi_05

    set gm_netw = sel_indt_aparc+aseg_REN_gm_GMI.nii.gz

    3dTrackID                             \
        -mode PROB                        \
        -dti_in   dt                      \
        -netrois  $gm_netw                \
        -uncert   dt_UNC.nii.gz           \
        -prefix   o.pr00                  \
        -nifti                            \
        -no_indipair_out                  \
        -dump_rois AFNI                   \
        -alg_Thresh_FA      0.2           \
        -alg_Thresh_Frac    0.1           \
        -alg_Nseed_Vox      5             \
        -alg_Nmonte      1000             \
        -echo_edu                         \
        -overwrite 

-> produces output in the same directory 'data_proc/SUBJ_001/dwi_05/',
since all volume files occupy the same space/grid...

**under construction**



Visualizing WM connections and **fat_proc_connec_vis**
------------------------------------------------------

**Proc:** This is an example of viewing several WMCs together in one
image that can be viewed in SUMA.  Each WMC is surfaceized and colored
individually.  In this case all AND-logic WMCs that were found through
the "Right-Thalamus-Proper" target are viewed::

    fat_proc_connec_vis                                            \
        -in_rois o.pr00/NET_000_ROI_*Right-Thalamus-Proper*nii.gz  \
        -prefix  o.pr00_Right-Thalamus-Proper                      \
        -trackid_no_or     

-> produces output in the same directory 'data_proc/SUBJ_001/dwi_05/',
since all volume files occupy the same space/grid...

**under construction**

.. list-table:: 
   :header-rows: 1
   :widths: 50 50

   * - **under construction**
     - (just axi and cor views)
   * - .. image:: media/postpre_iv/autorecord.A.171219_191503.265.jpg
          :width: 100%   
          :align: center
     - .. image:: media/postpre_iv/autorecord.A.171219_191508.798.jpg
          :width: 100%   
          :align: center
   * - .. image:: media/postpre_iv/autorecord.A.171219_191622.585.jpg
          :width: 100%   
          :align: center
     - .. image:: media/postpre_iv/autorecord.A.171219_191637.369.jpg
          :width: 100%   
          :align: center

Visualizing matrices of structural properties and **fat_proc_connec_vis**
------------------------------------------------------

**under construction**

**Proc:** This is an example of viewing several WMCs together in one
image that can be viewed in SUMA.  Each WMC is surfaceized and colored
individually.  In this case all AND-logic WMCs that were found through
the "Right-Thalamus-Proper" target are viewed::

    fat_proc_connec_vis                                            \
        -in_rois o.pr00/NET_000_ROI_*Right-Thalamus-Proper*nii.gz  \
        -prefix  o.pr00_Right-Thalamus-Proper                      \
        -trackid_no_or     

-> produces output in the same directory 'data_proc/SUBJ_001/dwi_05/',
since all volume files occupy the same space/grid...


.. 
    :

    .. list-table:: 
       :header-rows: 1
       :widths: 90

       * - Directory structure for example data set
       * - .. image:: media/postpre_iii/fp_13_roi_sel_make.png
              :width: 100%
              :align: center
       * - *Output files made to select only clumpy GM ROIs and then to
           perform controlled inflation.*

    |

    .. list-table:: 
       :header-rows: 1
       :widths: 20 80
       :stub-columns: 0

       * - Outputs of
         - the above ``3dcalc``/\ ``3drefit``/\ ``3dROIMaker``
       * - **sel_indt_aparc\*+aseg_REN_gm.nii.gz**
         - volumetric NIFTI file, 3D; the GM region map without the
           deselected ROIs. The same labeltable from the original input is
           contained within the file's header.
       * - **sel_indt_aparc\*+aseg_REN_gm_GMI.nii.gz**
         - volumetric NIFTI file, 3D; the output of ``3dROIMaker`` which
           contains the inflated map of ROIs.
       * - **sel_indt_aparc\*+aseg_REN_gm_GMI.niml.lt**
         - text file; the labeltable of the NIFTI file with the same root
           name.
       * - **sel_indt_aparc\*+aseg_REN_gm_GM.nii.gz**
         - volumetric NIFTI file, 3D; the output of ``3dROIMaker`` which
           contains the *non*\-inflated map of ROIs. (Having this output
           is useful if, for example, one uses ``3dROIMaker`` to subtract
           any regions from tissues, though that was not done here.)
       * - **sel_indt_aparc\*+aseg_REN_gm_GM.niml.lt**
         - text file; the labeltable of the NIFTI file with the same root
           name.

    |

    To view the dual points of 1) inflating the GM ROIs and 2)
    constraining that inflation, we show images of before-and-after
    inflation, for both the "2000" and "2009" parcellations.  The b/w ulay
    is the binary mask where FA>0.2, representing the DTI-based proxy for
    WM (and within which tracking normally occurs for healthy adult
    humans).  Note that in the pre-inflation cases, one can often see GM
    ROIs following the contours of the FA-WM, but there might be slight
    gaps due to either transformation, partial voluming, etc. Such regions
    might create artificial "misses" in the tracts, which don't leave the
    FA>0.2 boundaries to reach the GM they (possibly) should.  Conversely,
    in cases where the GM follows the FA-WM boundary well, we wouldn't
    want inflation pouring out into the WM unnecessarily.

    .. note:: When viewing the following montages, it might make sense to
              open corresponding montages of the inflated and non-inflated
              maps in browser tabs and then toggling views between them--
              that should highlight both of the main points.

    .. list-table:: 
       :header-rows: 1
       :widths: 50 50

       * - Images comparing the "2000" inflated and non-inflated GM maps 
         - (just axi and sag views)
       * - .. image:: media/postpre_iii/sel__qc2000_uFA02_gm.axi.png
              :width: 100%   
              :align: center
         - .. image:: media/postpre_iii/sel__qc2000_uFA02_gm.sag.png
              :width: 100%   
              :align: center

    .. list-table:: 
       :header-rows: 0
       :widths: 100

       * - *Non-inflated "2000" parc/seg map (after the non-regional ROIs
           were removed) olayed on FA>0.2 binary map ulay.*

    .. list-table:: 
       :header-rows: 0
       :widths: 50 50

       * - .. image:: media/postpre_iii/sel__qc2000_uFA02_GMI.axi.png
              :width: 100%   
              :align: center
         - .. image:: media/postpre_iii/sel__qc2000_uFA02_GMI.sag.png
              :width: 100%   
              :align: center

    .. list-table:: 
       :header-rows: 0
       :widths: 100

       * - *Inflated "2000" parc/seg map (after the non-regional ROIs were
           removed) olayed on FA>0.2 binary map ulay.*

    |


    .. list-table:: 
       :header-rows: 1
       :widths: 50 50

       * - Images comparing the "2009" inflated and non-inflated GM maps 
         - (just axi and sag views)
       * - .. image:: media/postpre_iii/sel__qc2009_uFA02_gm.axi.png
              :width: 100%   
              :align: center
         - .. image:: media/postpre_iii/sel__qc2009_uFA02_gm.sag.png
              :width: 100%   
              :align: center

    .. list-table:: 
       :header-rows: 0
       :widths: 100

       * - *Non-inflated "2009" parc/seg map (after the non-regional ROIs
           were removed) olayed on FA>0.2 binary map ulay.*

    .. list-table:: 
       :header-rows: 0
       :widths: 50 50

       * - .. image:: media/postpre_iii/sel__qc2009_uFA02_GMI.axi.png
              :width: 100%   
              :align: center
         - .. image:: media/postpre_iii/sel__qc2009_uFA02_GMI.sag.png
              :width: 100%   
              :align: center

    .. list-table:: 
       :header-rows: 0
       :widths: 100

       * - *Inflated "2009" parc/seg map (after the non-regional ROIs were
           removed) olayed on FA>0.2 binary map ulay.*
