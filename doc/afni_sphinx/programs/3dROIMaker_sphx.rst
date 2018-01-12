.. contents:: 
    :depth: 4 

**********
3dROIMaker
**********

.. code-block:: none

    
      ROIMaker, written by PA Taylor (Nov, 2012), part of FATCAT (Taylor & Saad,
      2013) in AFNI.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      THE GENERAL PURPOSE of this code is to create a labelled set of ROIs from
      input data. It was predominantly written with a view of aiding the process
      of combining functional and tractographic/structural data. Thus, one might
      input a brain map (or several, as subbricks) of functional parameters 
      (e.g., correlation coefficients or ICA maps of Z-scores), set a value 
      threshold and/or a cluster-volume threshold, and this program will find
      distinct ROIs in the data and return a map of them, each labelled with
      an integer. One can also provide a reference map so that, for example, in
      group studies, each subject would have the same number label for a given
      region (i.e., the L motor cortex is always labelled with a `2'). In order
      to be prepared for tractographic application, one can also enlarge the
      gray matter ROIs so that they intersect with neighboring white matter.
      One can either specify a number of voxels with which to pad each ROI, 
      and/or input a white matter skeleton (such as could be defined from a 
      segmented T1 image or an FA map) and use this as an additional guide for
      inflating the GM ROIs.  The output of this program can be used directly
      for guiding tractography, such as with 3dTrackID.
    
      If an input dataset ('-inset INSET') already contains integer delineation,
      such as using a parcellation method, then you can preserve these integers
      *even if the ROIs are contiguous* by using the same set as the reference
      set (-> '-refset INSET', as well).  Otherwise, contiguous blobs defined
      will likely be given a single integer value in the program.
    
      Labeltable functionality is now available.  If an input '-refset REFSET'
      has a labeltable attached, it will also be attached to the output GM and
      inflated GMI datasets by default (if you don't want to do this, you can
      use the '-dump_no_labtab' to turn off this functionality).  If either no
      REFSET is input or it doesn't have a labeltable, one will be made from
      zeropadding the GM and GMI map integer values-- this may not add a lot of
      information, but it might make for more useful output.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      OUTPUTS:
       + `GM' map of ROIs  :based on value- and volume-thresholding, would
                            correspond most closely to gray matter regions of
                            activation. The values of each voxel are an integer,
                            distinct per ROI.
       + `GMI' map of ROIs :map of inflated GM ROIs, based on GM map, with the 
                            ROIs inflated either by a user-designed number of
                            voxels, or also possibly including information of
                            the WM skeleton (so that inflation is halted after
                            encountering WM). The values of each voxel are the
                            same integers as in the GM map.
    
      + RUNNING, need to provide:
         -inset    INSET  :3D volume(s) of values, esp. of functionally-derived
                           quantities like correlation values or ICA Z-scores.
         -thresh   MINTHR :threshold for values in INSET, used to great ROI
                           islands from the 3D volume's sea of values.
         -prefix   PREFIX :prefix of output name, with output files being:
                           PREFIX_GM* and PREFIX_GMI* (see `Outputs', above).
        and can provide: 
         -refset   REFSET :3D (or multi-subbrick) volume containing integer 
                           values with which to label specific GM ROIs after
                           thresholding.  This can be useful to assist in having
                           similar ROIs across a group labelled with the same 
                           integer in the output GM and GMI maps.
                           If an INSET ROI has no corresponding REFSET label,
                           then the former is marked with an integer greater 
                           than the max refset label. If an INSET ROI overlaps
                           with multiple REFSET ROIs, then the former is split
                           amongst the latter-- overlap regions get labelled 
                           first, and then REFSET labels grow to cover the INSET
                           ROI in question.  NB: it is possible to utilize
                           negative-valued ROIs (voxels =-1) to represent NOT-
                           regions for tracking, for example.
         -volthr   MINVOL :integer number representing minimum size a cluster of
                           voxels must have in order to remain a GM ROI after 
                           the values have been thresholded.  Number might be
                           estimated with 3dAlphaSim, or otherwise, to reduce
                           number of `noisy' clusters.
         -only_some_top N :after '-volthr' but before any ref-matching or
                           inflating, one can restrict each found region
                           to keep only N voxels with the highest inset values.
                           (If an ROI has <N voxels, then all would be kept.)
                           This option can result in unconnected pieces.
         -only_conn_top N :similar-ish to preceding option, but instead of just
                           selecting only N max voxels, do the following
                           algorithm: start the ROI with the peak voxel; search
                           the ROI's neighbors for the highest value; add that
                           voxel to the ROI; continue until either the ROI has 
                           reached N voxels or whole region has been  added.
                           The returned ROI is contiguous and 'locally' maximal
                           but not necessarily globally so within the original
                           volume.
         -inflate  N_INFL :number of voxels which with to pad each found ROI in
                           order to turn GM ROIs into inflated (GMI) ROIs.
                           ROIs won't overlap with each other, and a WM skeleton
                           can also be input to keep ROIs from expanding through
                           a large amount of WM ~artificially (see below).
         -trim_off_wm     :switch to trim the INSET to exclude voxels in WM,
                           by excluding those which overlap an input WM
                           skeleton, SKEL (see `-wm_skel', below; to trim off
                           CSF, see separate `-csf_skel').  NB: trimming is done
                           before volume thresholding the ROIs, so fewer ROIs
                           might pass, or some input regions might be split
                           apart creating a greater number of regions.
         -wm_skel  SKEL   :3D volume containing info of WM, as might be defined
                           from an FA map or anatomical segmentation.  Can be
                           to guide ROI inflation with `-skel_stop'.
         -skel_thr THR    :if the skeleton is not a mask, one can put in a 
                           threshold value for it, such as having THR=0.2 if 
                           SKEL were a FA map.
         -skel_stop       :switch to stop inflation at locations which are 
                           already on WM skeleton (default: off; and need
                           `-wm_skel' to be able to use).
       -skel_stop_strict  :similar to '-skel_stop', but this also does not
                           allow any inflation *into* the skel-region.  The
                           '-skel_stop' let's the inflation go one layer
                           *into* the skel-region, so this is stricter. This
                           option might be my preference these days.
         -csf_skel CSF_SK :similar to SKEL, a 3D volume containing info of CSF.
                           NB: however, with CSF_SK, info must just be a binary
                           mask already, and it will only be applied in trimming
                           procedure (no affect on inflation); if input, INSET
                           is automatically trimmed of CSF, independent of
                           using `-trim_off_wm'.  Again, trimming done before
                           volume thresholding, so may decrease/separate regions
                           (though, that may be useful/more physiological).
         -mask   MASK     :can include a mask within which to apply threshold.
                           Otherwise, data should be masked already. Guess this
                           would be useful if the MINTHR were a negative value.
                           It's also useful to ensure that the output *_GMI*
                           ROI masks stay within the brain-- this probably won't
                           often matter too much.
                           For an N-brick inset, one can input an N- or 1-brick
                           mask.
        -neigh_face_only  : **DEPRECATED SWITCH** -> it's now default behavior
                           to have facewise-only neighbors, in order to be
                           consistent with the default usage of the clusterize
                           function in the AFNI window.
        -neigh_face_edge  :can loosen the definition of neighbors, so that
                           voxels can share a face or an edge in order to be
                           grouped into same ROI (AFNI default is that neighbors
                           share at least one edge).
        -neigh_upto_vert  :can loosen the definition of neighbors, so that
                           voxels can be grouped into the same ROI if they share
                           at least one vertex (see above for default).
        -nifti            :switch to output *.nii.gz GM and GMI files
                           (default format is BRIK/HEAD).
    
      -preinfl_inset PSET :as a possible use, one might want to start with a WM
                           ROI, inflate it to find the nearest GM, then expand
                           that GM, and subtract away the WM+CSF parts. Requires
                           use of a '-wm_skel' and '-skel_stop', and replaces
                           using '-inset'.
                           The size of initial expansion through WM is entered
                           using the option below; then WM+CSF is subtracted.
                           The *_GM+orig* set is returned. In the *_GMI+orig*
                           set, the number of voxels expanded in GM is set using
                           the '-inflate' value (WM+CSF is subtracted again
                           before output).
      -preinfl_inflate PN :number of voxels for initial inflation of PSET.
    
      -dump_no_labtab     :switch for turning off labeltable attachment to the
                           output GM and GMI files (from either from a '-refset
                           REFSET' or from automatic generation from integer
                           labels.
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
          3dROIMaker                     \
             -inset CORR_VALUES+orig.    \
             -thresh 0.6                 \
             -prefix ROI_MAP             \
             -volthr 100                 \
             -inflate 2                  \
             -wm_skel WM_T1+orig.        \
             -skel_stop_strict 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
