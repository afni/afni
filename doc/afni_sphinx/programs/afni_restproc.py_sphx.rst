****************
afni_restproc.py
****************

.. _ahelp_afni_restproc.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
      ** As of Dec 18, 2012, afni_restproc.py is (essentially) no longer being
         supported (by its author, Rayus Kuplicki of the University of Tulsa).
    
         Consider using afni_proc.py (written by Rick Reynolds of the NIH).
    
    
    afni_restproc.py 
    
    This script takes care of preprocessing commonly done prior to resting state
    analyses. The process is similar to ANATICOR and RETROICOR and consists of 
    removing a number of regressors of noninterest. Some regressors need to be
    computed prior to running this script, others do not. These regressors include
    all or a subset of (with requirements in parentheses):
    
            motion parameters (-align on, default)
            local or global WM signal (supply appropriate mask or segmentation file)
            average ventricle signal (supply appropriate mask or segmentation file)
            average whole brain signal (-includebrain)
            RVT signal (compute first using RetroTS.m)
            arbitrary ROI average regressors (supply masks)
            other arbitrary regressors supplied by the user (compute first)
    
    Additionally, this script takes care of alignment, registration, and slice
    timing correction via align_epi_anat.py.  It will also talairach the data if 
    desired-either before or after processing, your choice. Additionally, this 
    script implements two censoring methods.  One is based on outliers and the 
    other is identical to the method used by Power et al. (Neuroimage 2012)
    
    
    Required Options:
            -anat aa        :aa is the high resolution anatomy file
            -epi ee         :ee is the epi timeseries to process
            
    Optional Options:
    
            Informational Options:
                    -help           :Display this help message
                    -changelog      :Display a log of changes to this script
    
            Output Options:
                    -prefix p       :Prepend p to each of the final output files.  
                                    Default is rest_proc.
                    -dest d         :Put the results in directory d. Default is to
                                    match p.
                    -tsnr           :Compute the tSNR of the EPI as described below.
                    -snr n c        :Compute the SNR of the EPI. n should be a scan
                                    collected with the RF turned off. This will look
                                    like static on an old TV and is used to estimate
                                    the variance of the noise inherent in the
                                    system. c should be the number of channels in 
                                    the coil you used and will determine a
                                    correction factor.
                    -corrmap        :Run 3dTcorrMap on the clean data.
                    -corrmapt t     :Use t as the threshold when computing average 
                                    correlation strengths. Default is 0.3. The idea
                                    here is that you may be interested in the
                                    average correlation between each voxel and all
                                    other voxels it is connected to, but below
                                    a certain threshold two voxels could be
                                    considered disconnected, so discard those weak
                                    correlations. Regardless of what t is, mean
                                    correlations without thresholding are also 
                                    stored.
                    -script sc      :Write all commands to a script named sc. This
                                    script can be modified and run later, similar
                                    to the output from afni_proc.py
    
            Alignment Options:
                    -tlrc           :Do the preprocessing in talairach space.  
                                    Default is to stay in orig space.
                    -tlrclast       :Do all preprocessing in orig space, but then 
                                    talairach the results. Pick either -tlrc or 
                                    -tlrclast (or neither).
                    -episize mm     :Cubic voxel size of all datasets (other than 
                                    the anatomy) after transforming them to 
                                    talairach space. Default is 2. This only works
                                    with -tlrclast or -tlrc.
                    -align [on]/off :Do the Alignment etc. step. Turn this option 
                                    off if all of the datasets are already aligned.
                    -alignbase b    :Align both the epi and anat datasets to b.  
                                    b should be an epi dataset and the first image
                                    will be used for alignment. This option makes 
                                    sense if you have several epi runs and you want
                                    all of them aligned to the same base.  Also, 
                                    this option only makes sense when not using 
                                    -tlrc or -epi2anat (-tlrclast is ok).
                    -epi2anat       :Align epi to anat instead of anat to epi.  
                                    This only makes sense when not using -tlrc.
                    -uniformize     :Uniformize anat before alignment.  Sometimes 
                                    This helps with skull stripping problems.
                    -anat_has_skull [yes]/no
                                    :Set this option to no if the anatomy has
                                    already been skull stripped (useful when default
                                    skull stripping doesn't work right).
    
            Regressor Options:
                    -aseg a         :a is the aseg segmentation file from 
                                    freesurfer. It should be aligned with the
                                    anatomy supplied as -anat and can be in .mgz,
                                    .nii, or .BRIK format.
                    -wmsize w       :Radius (in mm) of the sphere to use when
                                    computing the local white matter regressors.
                                    Default is 15mm
                    -globalwm       :Use the global wm average as a single 
                                    regressor instead of computing local wm 
                                    regressors.
                    -venterode v    :Number of nonmask neighbors required to cause 
                                    erosion in the ventricles. Default is 2
                    -wmerode we     :Number of nonmask neighbors required to cause 
                                    erosion in the WM mask. Default is 1.
                    -rvt r          :r is the RVT file produced by RetroTS.m
                    -includebrain   :Include the whole brain average regressor.
                    -dreg           :Add the derivatives of all regressors as 
                                    regressors.
                    -regressor re   :Use re as a regressor. re will be processed 
                                    in the same way the other regressors are
                                    (detrended, catenated). If you do alignment 
                                    and registration outside of this script, it may
                                    be a good idea to provide the motion parameters
                                    as a regressor. re can be either a 3d+t volume
                                    (specifying a different regressor for each
                                    voxel) or a .1D file (specifying a single global
                                    regressor).
                    -globalregmask g:Use the average signal extracted from the mask
                                    g as a global regressor of noninterest. This 
                                    will produce one regressor used for all voxels.
                    -localregmask rm rr
                                    :Use the local average signal extracted from
                                    rm as a regressor of noninterest. This will 
                                    produce a different regressor for each voxel.
                                    rm should be a mask defining the ROI to use
                                    and rr is the radius in mm to use when computing
                                    local average signals.
                                    -regressor, -globalregmask and -localregmask
                                    can be used multiple times to supply an
                                    arbitrary number of regressors.
    
            Censoring Options:
                    -outcensor      :Censor timepoints based on their number of
                                    outliers and head motion magnitude. Censored
                                    time points are cut out.
                    -fraclimit f    :When using -outcensor, fraction of voxels
                                    identified as outliers needed to censor a time
                                    point. Default is 0.05.
                    -motlimit m     :When using -outcensor, limit on rms motion to
                                    censor a point. Default is 0.3.
                    -dvarscensor    :Create a censor file based on FD (framewise
                                    displacement) and DVARS as from Power et. al, 
                                    Neuroimage 2012. 
                    -fdlimit ff     :Set the FD limit to be ff
                    -dvarslimit dd  :Set the DVARS limit to be dd
                    -censorleft s   :Censor s steps to the left of bad time points.
                                    Default is 1.
                    -censorright ss :Censor ss steps to the right of bad time 
                                    points. Default is 2.
                    -censorunion    :Censor the union of fraclimit and motlimit or
                                    FD and DVARS, instead of the intersection.
                    -keepuncensored :Keep a copy of the uncensored timeseries. It
                                    will be called 
                                    [prefix].cleanEPI.uncensored+[view]
    
            Normalization Options:
                    -localnorm      :Normalize based on voxelwise mean
                    -globalnorm     :Normalize based on global mean 
                    -modenorm       :Normalize based on global mode using 100 bins
                    -normval n      :Scale the selected attribute to be n
    
            Smoothing Options:
                    -smooth [on]/off:Smooth the clean timeseries data.
                    -smoothrad s    :FWHM size of smoothing to apply after cleaning
                                    the data.  Default is 4mm. Smoothing is done
                                    using a grey/nongrey matter mask by default.
                    -smoothtogether :Smooth everything inside a brain mask together,
                                    rather than smoothing the grey/nongrey matter 
                                    separately.
                    -smoothfirst    :Smooth the data before doing regression,
                                    instead of after.
    
            Misc. Processing Options:
                    -despike[on]/off:Despike the timeseries as the fist 
                                    preprocessing step.
                    -trcut t        :Number of TRs to throw away.  Default is 4.
                    -polort p       :Polynomial to detrend from the regressors and 
                                    the timeseries.  Similar to 3dDeconvolve  
                                    -polort A, default is floor(1 + TR*nVOLS / 150).
                    -bandpass       :Do bandpass filtering with LHz < f < HHz. 
                                    Default is 0.009 and 0.08.
                    -setbands L H   :Set L and H for bandpass filtering
                    -bpassregs      :Also bandpass filter the regressors.
                    -exec [on]/off  :Execute the commands. Turn this off and use 
                                    -script to get things setup without running 
                                    anything.
    
            Other Options:
                    -apply_censor e c p
                                    :This option is used to apply a censor file to
                                    remove timepoints from a timeseries. If it is
                                    given, this is the only option that will be
                                    processed. e is the timeseries to censor. c is
                                    a 1D file consisting of a single column of
                                    0's and 1's which must be the same length as e.
                                    Time points with 1's in c will be kept, 0's will
                                    be discarded. p is the prefix to use for the
                                    output timeseries.
    
    The following steps should be done before running this script:
            Create anatomical regressor masks:
                    If you want to remove anatomical regressors or noninterest,
                    the average ventricle signal, for example, you will need to 
                    provide masks used to extract these signals. This can be done 
                    in two different ways. Either supply the aseg file produced by
                    freesurfer, which is used to extract the ventricle and white
                    matter ROIs, or supply your own arbitrary masks with the 
                    -globalregmask or -localregmask options.
            Align the segmentation file with the experimental anatomy:
                    If given, The aseg file from freesurfer (it can be in mgz, nii,
                    or BRIK format) is assumed to be in alignment with the 
                    experimental anatomy.  The aseg and anat files will already be
                    aligned if the anatomy is the one used by freesurfer.  If it is
                    not, you may need to use something like @SUMA_AlignToExperiment.
            Create the RVT file:
                    This is done by processing the experiment's cardiac and 
                    respiratory files using RetroTS.m, available in the AFNI Matlab
                    library. While it is probably beneficial to remove the
                    estimated cardiac and respiratory signals, this step is not
                    necessary and this script will run fine without them.
    
    Processing is done in the following steps:
            Copy Files:
                    The output directory (specified by -dest) is created and input
                    files are copied to dest/tmp
            Despike:
                    This step is done first, if at all, so that spikes are not
                    'smeared around' by registration and slice timing correction
            Alignment etc.:
                    This step aligns the epi and anat datasets while also taking 
                    care of slice timing correction and the talairach 
                    transformation, if requested. These steps are combined using
                    align_epi_anat.py to minimize the number of interpolations 
                    required. If processing is done in +orig space, the anat is
                    aligned to the epi by default. Using -epi2anat will cause the
                    epi to be aligned to the anat. The appropriate transformation
                    is also applied to the aseg file and any masks provided by 
                    -localregmask and -globalregmask to keep them aligned with the 
                    anat.
            tcat:
                    This is where the first few time points are thrown out.  This 
                    step is delayed until after alignment so that the first high
                    contrast epi image can be used in the alignment process.
            tSNR:
                    At this stage in processing the tSNR of the EPI data is 
                    computed if requested.  It is taken to be  
                    mean(pEPI) / stdev(det(pEPI)) where pEPI is the processed EPI 
                    (despiked, aligned, tshifted, catenated) and det represents 
                    detrending with polynomial order polort.
            SNR:
                    If you have collected a scan with the RF turned off (used to
                    estimate the varience of the noise) the SNR can be computed
                    for each voxel. It is taken to be S/(sigma*corr) where S is 
                    the signal in the first frame of the epi timeseries, sigma is
                    the standard deviation of the values in the noise scan, and 
                    corr is a correction factor based on the number of channels in
                    the coil. After computing the SNR of the original EPI volume,
                    it is transformed to be aligned with and have the same voxel
                    size as the final EPI. Correction factors are:
                            corr1 = 1.5263997
                            corr8 = 1.4257312
                            corr16 = 1.4198559
                            corr32 = 1.4170053
            Normalize EPI:
                    If a method of normalization is chosen, it is applied here.
                    Normalization is done to scale the selected attribute to be
                    -normval inside a brain mask. Specifically:
                    -globalnorm computes the global mean signal across time and
                    space in the brain and scales accordingly. 
                    normval * (voxel intensity)/(global mean)
                    -modenorm computes the global mode intensity across time and
                    space in the brain and scales accordingly.
                    normval * (voxel intensity)/(global mode)
                    -localnorm computes the temporal average for each voxel and
                    scales accordingly.
                    normval * (voxel intensity)/(voxel mean)
            Prep WM Mask:
                    The WM mask is taken from the aseg file from freesurfer.  It is
                    first taken to be all voxels with labels: 2,7,16,41,46,251,252,
                    253,254,255. This mask is resampled to match the resolution of
                    the epi dataset. After resampling, the mask is eroded so that
                    it is less likely to contain any grey matter. By default, 
                    typical erosion is done which removes voxels which have a 
                    single non-mask neighbor from the mask.
            Prep Ventricle Mask:
                    The ventricle mask is taken from the aseg file using labels 4 
                    and 43. This mask is resampled to match the epi and then eroded.
                    Erosion of the ventricle mask is by default less conservative
                    than typical erosion. Voxels require two neighbors to be 
                    non-mask voxels in order to be eroded. This is done because a 
                    large number of subjects end up without any voxels in the 
                    ventricle mask using standard erosion and a 64x64 matrix. If
                    your data are higher resolution, you may want to use -venterode
                    1. It is a good idea to check both the WM and ventricle masks 
                    to make sure they look good.
            Prep Blurring Mask:
                    The last step (after regression) is to apply gaussian smoothing.
                    By default (if -aseg was specified), this smoothing is done in
                    the grey and nongrey matter seperately via 3dBlurInMask. The
                    blurring mask is created so that grey matter voxels are labeled 1
                    and nongrey voxels (inside the brain) are labeld 2. The labeling
                    is simply (automask + WM mask + Vent mask). If -aseg was not
                    specified, or -smoothtogether was given, the smoothing is done
                    using the whole brain as one region.
            Smoothing:
                    If -smoothfirst was selected, this is where smoothing takes
                    place. It is done as described below.
            Extract Regressors from masks:
                    Regressor timeseries are extracted from the WM and ventricle
                    masks as well as any masks supplied by the user with the
                    -globalregmask and -localregmask options. A single regressor
                    timeseries is computed as the average value for each supplied
                    global masks and the ventricle mask. For the WM mask and any
                    masks supplied with -localregmask, different regressors are
                    computed for each voxel. For a single voxel, the regressor is
                    defined as the average timeseries in voxels which are both
                    within the supplied radius and included in the mask.
                    -globalregmask and -localregmask are useful if you want to use
                    software other than Freesurfer for the segmentation step. For
                    example, using whatever method you like, you can create a
                    ventricle mask and supply it as a global mask. Likewise, you can
                    create a whitematter mask and supply it as a local regressor.
                    Multiple local and global masks can be supplied.
            Differentiate Regressors:
                    If desired, the temporal derivatives of each regressor are 
                    computed and added to the list of regressors.
            Detrend Regressors:
                    The polynomial of order -polort is removed from each of the
                    regressors (RVT, WM, Vent, Motion). This is done so there are 
                    no competing polynomial terms during the regression step.
            Bandpass Filtering:
                    If bandpass filtering is selected, it is applied to the EPI 
                    data here after regression. This is also where the regressors 
                    are bandpass filtered if -bpassregs was selected.
            Regression:
                    The regressors of noninterest (RVT,WM,Vent,Motion,other
                    arbitrary regressors) are taken out of the epi timeseries using
                    3dTfitter, which also removes the polynomial selected using
                    -polort.
            Create Censor File:
                    If a censoring method was chosen, the offending time points are
                    identified here. 
                    -outcensor:
                            Make a temporal mask marking frames with more outliers
                            than the threshold specified by -fraclimit. 
                            Make a temporal mask marking frames with more RMS
                            motion than specified by -motlimit.
                            Mark frames -censorleft and -censorright steps to the 
                            left and right of time points flagged in the two masks.
                            Take the intersection of the two masks created above
                            (or the union, if -censorunion was specified)
                    -dvarscensor:
                            Make a temporal mask marking frames with FD greater
                            than the threshold specified by -fdlimit.
                            Make a temporal mask marking frames with DVARS greater
                            than the threshold specified by -dvarslimit.
                            Mark frames -censorleft and -censorright steps to the 
                            left and right of time points flagged in the two masks.
                            Take the intersection of the two masks created above
                            (or the union, if -censorunion was specified)
            Smoothing:
                    By default, 3dBlurInMask is used to smooth the timeseries in 
                    the grey and nongrey matter separately. Grey matter voxels are
                    likely the interesting ones, but it can't hurt to apply the 
                    same process to nongrey voxels to see what they look like. The 
                    -smoothtogether flag can be used to apply uniform smoothing to
                    all voxels in the brain instead. -smooth off skips this step.
            Censoring:
                    If a censoring method was chosen, the censored time points are
                    removed here.
            TcorrMap:
                    At this point, the data have been preprocessed to remove
                    uninteresting signals. It is now appropriate to do resting 
                    state functional connectivity analysis on the clean data. One
                    thing to examine is the result of running 3dTcorrMap. For a 
                    full description of what this does, see the help from
                    3dTcorrMap. This script uses it as follows:
                            3dTcorrMap -input cleanEPI -mask automask -polort -1 
                                    -mean prefix.MeanCorr -Hist 400 prefix.CorHist 
                                    -Cexpr 'step(r-t)*r' prefix.MeanCorrGT
                    where t can be specified using -corrmapt and is 0.3 by default. 
    
    Things to check after running this script:
            Alignment:
                    Make sure the various mask datasets are in good alignment with
                    the anatomical dataset.
            Mask Coverage:
                    Make sure the ventricle and white matter masks cover what you
                    think are appropriate voxels.
    
    
    
    Example Usage:
    
    
            #Basic usage:
            #Remove RVT, motion parameters, WM and ventricle signals from 
            #epi+orig
            #Store the results in a directory named preproc
            #Prefix each result file with subjX
            #Processing is done in orig space
            afni_restproc.py -anat mprage+orig. \
                    -epi epi+orig. \
                    -rvt RVT.slibase.1D \
                    -aseg aseg.mgz \
                    -dest preproc \
                    -prefix subjX 
    
            #Produce a tsnr map and results from 3dTcorrMap using a threshold of .15
            #Write a script called proc.tcsh but don't execute it yet
            #This script can be modified and executed at your leisure
            afni_restproc.py -anat mprage+orig. \
                    -epi epi+orig. \
                    -rvt RVT.slibase.1D \
                    -aseg aseg.mgz \
                    -dest preproc \
                    -prefix subjX \
                    -corrmap \
                    -corrmapt .15 \
                    -tsnr \
                    -script proc.tcsh \
                    -exec off
    
            #Alignment and talairaching were done already, so skip those steps
            #Use the provided motion parameter file as a regressor
            afni_restproc.py \
                    -epi epi+tlrc \
                    -rvt RVT.slibase.1D \
                    -anat mprage+tlrc \
                    -aseg aseg+tlrc \
                    -regressor epi_tsh_vr_motion.1D \
                    -dest prealigned \
                    -prefix subjX \
                    -align off 
    
            #Do processing like it was done in Power et al. Neuroimage 2012
            afni_restproc.py \
                    -despike off \
                    -aseg aseg.mgz \
                    -anat mprage+orig \
                    -epi rest+orig \
                    -script power_method.tcsh \
                    -dest power_method_subjx \
                    -prefix pm \
                    -dvarscensor \
                    -tlrc \
                    -episize 3 \
                    -dreg \
                    -smoothfirst \
                    -smoothrad 6 \
                    -smoothtogether \
                    -bandpass \
                    -includebrain \
                    -polort 0 \
                    -globalwm \
                    -censorleft 1 \
                    -censorright 2 \
                    -fdlimit 0.5 \
                    -dvarslimit 5 \
                    -modenorm 
    
            #Apply a censor file to a timeseries.  This will output a file called
            #epi.censored+orig that has TRs cut out wherever censor.1D is 0.
            afni_restproc.py \
                    -apply_censor \
                    epi+orig \
                    censor.1D \
                    epi.censored
    
    Original version by Rayus Kuplicki.
    University of Tulsa
    Laureate Institute for Brain Research
    Report problems or feature requests to rkuplicki@laureateinstitute.org.
    12-18-12
