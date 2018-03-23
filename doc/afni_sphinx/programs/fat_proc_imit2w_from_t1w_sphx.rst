.. _ahelp_fat_proc_imit2w_from_t1w:

************************
fat_proc_imit2w_from_t1w
************************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    
      Some basic processing of T1w anatomical images, particularly for
      preparation in using as a reference structural in TORTOISE -> makes
      an imitation T2w-contrast image, in terms of relative tissue
      contrast.  Make sure to verify all results visually!
    
      This does: unifizing of brightness, anisosmoothing, some skull
      stripping, and also generates an imitation T2w-contrast image
      through **very** simple means.  The output T2w volume is *not* for
      quantitative use, but for registrative purposes.
    
      Some automatic QC images are generated now, as well.  Montages of
      axial, sagittal and coronal views of the final T2w volume are saved
      by default in the same location as the output volumes.
    
      *NB: if you use a volume made this way as a reference in TORTOISE,
      then at least for ~adult-human-like (imitation) t2w volumes, at
      present it seems like you should use the following option when:
      running DR_BUDDI:  --enforce_deformation_antisymmetry 1.
      This seems to improve registration.
    
    
      REQUIRES: AFNI.
    
      Ver. 2.2 (PA Taylor, Mar 1, 2018)
    
      For use, example images, and citation, see (esp. Appendix A):
         Taylor PA, Alhamud A, van der Kouwe AJ, Saleh MG, Laughton B,
         Meintjes EM.  Assessing the performance of different DTI motion
         correction strategies in the presence of EPI distortion
         correction.  Hum Brain Mapp (in press).
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      This script has one required argument ('-inset ...'), and the rest are
        optional:
    
        fat_proc_imit2w_from_t1w  \
            -inset  T1_FILE                       \
            -prefix PPP                           \
            {-workdir WWW}                        \
            {-mask    MASK}                       \
            {-ss_blur_fwhm BBB}                   \
            {-no_clean}                           \
            {-no_qc_view}                         \
            {-qc_prefix QCP}
    
      where: 
      -inset  T1_FILE  :is the full name of the input T1w volume;
    
      -prefix  PPP     :output prefix for files and snapshots (required).
    
      -mask   MASK     :an optional input of a pre-skullstripped T1_FILE
                        (this can be either a mask or a skullstripped volume).
                        This can be useful if the default skullstripping
                        options in this script ain't getting the job done
                        and other ones have to be done (skullstripping is
                        probably the slowest part of this set of steps).
    
     -ss_blur_fwhm BBB :optional, can add in blurring during the 3dSkullStrip
                        part of this program, in units of mm (default FWHM: 2).
    
      -workdir WWW     :specify a working directory, which can be removed;
                        (default name = '__WORKING_imit2w_from_t1w')
    
      -no_qc_view      :turn off the automatic creation of QC montages (which
                        are produced by default).
    
      -qc_prefix QCP   :change the prefix of the QC images (default: use the
                        prefix of the volumes).
    
      -no_clean        :is an optional switch to NOT remove working directory
                        '__WORKING_imit2w_from_t1w'; (default: remove working dir).
    
     ------------------------------------------------------------------------
    
      OUTPUTS:
    
        PREFIX.nii.gz         :a volume with T2w-like tissue contrast made
                               from a T1w one; the outside of the brain
                               has scaled skull and noise, for having a
                               non-zero SNR estimation.
        PREFIX_orig.nii.gz    :a somewhat cleaned/processed version of the
                               input T1w volume; it also has a scaled skull 
                               and noise outside the brain.
        PREFIX_orig_ss.nii.gz :a skull-stripped version of PREFIX_t1w.nii.gz.
    
        PREFIX_qc*
                              :QC images of the skull-stripped T1w volume
                               and of the final imitation-T2w volume.
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
        
        fat_proc_imit2w_from_t1w   \
            -inset T1.nii.gz               \
            -prefix imit2w
      or
    
        fat_proc_imit2w_from_t1w   \
            -inset T1.nii.gz               \
            -mask  mask_WB.nii.gz          \
            -prefix imit2w
            -no_clean
    
    -------------------------------------------------------------------------
