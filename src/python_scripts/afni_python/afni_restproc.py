#!/usr/bin/python

import sys
import os
import option_list as OL
import random
from afni_base import *
from db_mod import *
from math import pow

rickr_note = """
  ** As of Dec 18, 2012, afni_restproc.py is (essentially) no longer being
     supported (by its author, Rayus Kuplicki of the University of Tulsa).

     Consider using afni_proc.py (written by Rick Reynolds of the NIH).
"""
     

help_string = """
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
        afni_restproc.py -anat mprage+orig. \\
                -epi epi+orig. \\
                -rvt RVT.slibase.1D \\
                -aseg aseg.mgz \\
                -dest preproc \\
                -prefix subjX 

        #Produce a tsnr map and results from 3dTcorrMap using a threshold of .15
        #Write a script called proc.tcsh but don't execute it yet
        #This script can be modified and executed at your leisure
        afni_restproc.py -anat mprage+orig. \\
                -epi epi+orig. \\
                -rvt RVT.slibase.1D \\
                -aseg aseg.mgz \\
                -dest preproc \\
                -prefix subjX \\
                -corrmap \\
                -corrmapt .15 \\
                -tsnr \\
                -script proc.tcsh \\
                -exec off

        #Alignment and talairaching were done already, so skip those steps
        #Use the provided motion parameter file as a regressor
        afni_restproc.py \\
                -epi epi+tlrc \\
                -rvt RVT.slibase.1D \\
                -anat mprage+tlrc \\
                -aseg aseg+tlrc \\
                -regressor epi_tsh_vr_motion.1D \\
                -dest prealigned \\
                -prefix subjX \\
                -align off 

        #Do processing like it was done in Power et al. Neuroimage 2012
        afni_restproc.py \\
                -despike off \\
                -aseg aseg.mgz \\
                -anat mprage+orig \\
                -epi rest+orig \\
                -script power_method.tcsh \\
                -dest power_method_subjx \\
                -prefix pm \\
                -dvarscensor \\
                -tlrc \\
                -episize 3 \\
                -dreg \\
                -smoothfirst \\
                -smoothrad 6 \\
                -smoothtogether \\
                -bandpass \\
                -includebrain \\
                -polort 0 \\
                -globalwm \\
                -censorleft 1 \\
                -censorright 2 \\
                -fdlimit 0.5 \\
                -dvarslimit 5 \\
                -modenorm 

        #Apply a censor file to a timeseries.  This will output a file called
        #epi.censored+orig that has TRs cut out wherever censor.1D is 0.
        afni_restproc.py \\
                -apply_censor \\
                epi+orig \\
                censor.1D \\
                epi.censored

Original version by Rayus Kuplicki.
University of Tulsa
Laureate Institute for Brain Research
Report problems or feature requests to rkuplicki@laureateinstitute.org.
12-18-12
"""
        
change_string = """
Changelog for afni_restproc.py:
4-26-12
Initial version

5-4-12
fixed -dreg so now the derivatives of the regressors are computed and used 
        properly
if -smoothfirst is chosen, smoothing now takes place before computing regressors
removed import subprocess
fixed placement of comment about copying results

8-10-12
fixed a bug with -outcensor where neighbors of timepoints flagged for excessive
        motion were not also being flagged for censoring
fixed a bug with -snr where the computed snr map was not copied to the result
        directory
added -bpassregs
added -keepuncensored
-tsnr now detrends with polynomial order polort instead of 4

10-26-12
fixed a bug where using input files that had already been tshifted would result
        in a filename error
added an error message when the user specifies an input file that does not exist 
fixed comment placement for normalization block
fixed a naming conflict when using both -tsnr and -localnorm, -globalnorm, or 
        -modenorm
added -uniformize
added -anat_has_skull

12-18-12
.nii (and .mgz) input files are now handled properly
"""
class RestInterface:
   """Rest Processing Interface Class"""
   def __init__(self, verb=1):
      self.init_options()
      self.oexec = ""
      self.prefix = "rest_proc"
      self.dest = "rest_proc"
      self.anat = None
      self.epi = None
      self.rvt = None
      self.aseg = None
      self.despike = True
      self.tlrc = False
      self.wmsize = "15"
      self.globalwm = False
      self.venterode = "2"
      self.wmerode = "1"
      self.smoothrad = "4"
      self.polort = "-1"
      self.trcut = "4"
      self.tsnr = False
      self.corrmap = False
      self.corrmapt = "0.3"
      self.align = True
      self.regressors = []
      #list of regmasks.  each element is either a string, indicating a global regressor mask, or a list contianing two strings
      #indicating a local regressor mask and the size to use with it.
      self.regmasks = []
      self.dreg = False
      self.script = None
      self.execute = True
      self.epi2anat = False
      self.smoothtogether = False
      self.tlrclast = False
      self.smoothepi = True
      self.episize = 2
      self.alignbase = None
      self.outcensor = False
      self.fraclimit = "0.05"
      self.motlimit = "0.3"
      self.fdlimit = "0.5"
      self.sfdlimit = "0.2"
      self.dvarslimit = "5"
      self.censorleft = "1"
      self.censorright = "2"
      self.scensorleft = "1"
      self.scensorright = "2"
      self.censorunion = False
      self.dvarscensor = False
      self.includebrain = False
      self.smoothfirst = False
      self.bandpass = False
      self.bandl = "0.009"
      self.bandh = "0.08"
      self.bpassregs = False
      self.localnorm = False
      self.globalnorm = False
      self.modenorm = False
      self.uniformize = False
      self.normval = "1000"
      self.noise = None
      self.channels = None
      self.keepuncensored = False
      self.ss = True


   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')
      #required options
      self.valid_opts.add_opt('-anat', 1, [], req=1, helpstr='anatomy dataset')
      self.valid_opts.add_opt('-epi', 1, [], req=1, helpstr='timeseries to process')


      #optional input
      self.valid_opts.add_opt('-prefix', 1, [], req=0, helpstr='output prefix')
      self.valid_opts.add_opt('-help', 0, [], req=0, helpstr='help')
      self.valid_opts.add_opt('-changelog', 0, [], req=0, helpstr='changelog')
      self.valid_opts.add_opt('-aseg', 1, [], req=0, helpstr='freeserfer segmentation file')
      self.valid_opts.add_opt('-rvt', 1, [], req=0, helpstr='prepared RVT file')
      self.valid_opts.add_opt('-despike', 1, [], req=0, helpstr='despike')
      self.valid_opts.add_opt('-tlrc', 0, [], req=0, helpstr='talairach at the beginning')
      self.valid_opts.add_opt('-wmsize', 1, [], req=0, helpstr='white matter sphere size')
      self.valid_opts.add_opt('-globalwm', 0, [], req=0, helpstr='use the global wm average')
      self.valid_opts.add_opt('-venterode', 1, [], req=0, helpstr='number of neighbors for erosion')
      self.valid_opts.add_opt('-wmerode', 1, [], req=0, helpstr='number of neighbors for erosion')
      self.valid_opts.add_opt('-smoothrad', 1, [], req=0, helpstr='radius for gaussian smoothing')
      self.valid_opts.add_opt('-polort', 1, [], req=0, helpstr='polort to detrend from regressors')
      self.valid_opts.add_opt('-trcut', 1, [], req=0, helpstr='number of TRs to cut off')
      self.valid_opts.add_opt('-dest', 1, [], req=0, helpstr='dir to put results in')
      self.valid_opts.add_opt('-tsnr', 0, [], req=0, helpstr='compute tsnr map')
      self.valid_opts.add_opt('-snr', 2, [], req=0, helpstr='compute snr map')
      self.valid_opts.add_opt('-corrmap', 0, [], req=0, helpstr='use 3dTcorrMap')
      self.valid_opts.add_opt('-corrmapt', 1, [], req=0, helpstr='set threshold for 3dTcorrMap')
      self.valid_opts.add_opt('-align', 1, [], req=0, helpstr='do alignment and tshifting')
      self.valid_opts.add_opt('-anat_has_skull', 1, [], req=0, helpstr='specify if anatomy has skull')
      self.valid_opts.add_opt('-regressor', 1, [], req=0, helpstr='another regressor to remove')
      self.valid_opts.add_opt('-globalregmask', 1, [], req=0, helpstr='mask to use to extract a global regressor')
      self.valid_opts.add_opt('-localregmask', 2, [], req=0, helpstr='mask to use to extract a local regressor, and size')
      self.valid_opts.add_opt('-dreg', 0, [], req=0, helpstr='add regressor derivatives')
      self.valid_opts.add_opt('-script', 1, [], req=0, helpstr='write the commands to a file')
      self.valid_opts.add_opt('-exec', 1, [], req=0, helpstr='execute the commands')
      self.valid_opts.add_opt('-epi2anat', 0, [], req=0, helpstr='align epi to anat')
      self.valid_opts.add_opt('-smoothtogether', 0, [], req=0, helpstr='smooth using whole brain mask')
      self.valid_opts.add_opt('-tlrclast', 0, [], req=0, helpstr='talairach at the end')
      self.valid_opts.add_opt('-smooth', 1, [], req=0, helpstr='do not smooth the clean EPI data')
      self.valid_opts.add_opt('-episize', 1, [], req=0, helpstr='final size of epi voxels')
      self.valid_opts.add_opt('-alignbase', 1, [], req=0, helpstr='epi to align to')
      self.valid_opts.add_opt('-outcensor', 0, [], req=0, helpstr='create a censor file')
      self.valid_opts.add_opt('-fraclimit', 1, [], req=0, helpstr='fraction of points labeled outliers that causes censoring')
      self.valid_opts.add_opt('-motlimit', 1, [], req=0, helpstr='motion limit for censoring')
      self.valid_opts.add_opt('-dvarscensor', 0, [], req=0, helpstr='use Power censoring')
      self.valid_opts.add_opt('-fdlimit', 1, [], req=0, helpstr='FD limit for censoring')
      self.valid_opts.add_opt('-sfdlimit', 1, [], req=0, helpstr='FD limit for censoring')
      self.valid_opts.add_opt('-dvarslimit', 1, [], req=0, helpstr='DVARS limit for censoring')
      self.valid_opts.add_opt('-censorleft', 1, [], req=0, helpstr='num points to mask left')
      self.valid_opts.add_opt('-censorright', 1, [], req=0, helpstr='num of points to mask right')
      self.valid_opts.add_opt('-scensorleft', 1, [], req=0, helpstr='num points to mask left for strict motion')
      self.valid_opts.add_opt('-scensorright', 1, [], req=0, helpstr='num of points to mask right for strict motion')
      self.valid_opts.add_opt('-censorunion', 0, [], req=0, helpstr='censor union of FD and DVARS')
      self.valid_opts.add_opt('-keepuncensored', 0, [], req=0, helpstr='keep a copy of the uncensored timeseries')
      self.valid_opts.add_opt('-smoothfirst', 0, [], req=0, helpstr='smooth before regression')
      self.valid_opts.add_opt('-bandpass', 0, [], req=0, helpstr='do bandpass filtering')
      self.valid_opts.add_opt('-bpassregs', 0, [], req=0, helpstr='do bandpass filtering on regressors')
      self.valid_opts.add_opt('-setbands', 2, [], req=0, helpstr='set bandpass cutoffs')
      self.valid_opts.add_opt('-includebrain', 0, [], req=0, helpstr = 'include whole brain regressor')
      self.valid_opts.add_opt('-localnorm', 0, [], req=0, helpstr = 'normalize epi based on local mean')
      self.valid_opts.add_opt('-globalnorm', 0, [], req=0, helpstr = 'normalize epi based on global mean')
      self.valid_opts.add_opt('-modenorm', 0, [], req=0, helpstr = 'normalize epi based on global mode')
      self.valid_opts.add_opt('-uniformize', 0, [], req=0, helpstr = 'uniformize input anat')
      self.valid_opts.add_opt('-normval', 1, [], req=0, helpstr = 'normalize selected attribute to be normval')

   def process_options(self):
      if len(sys.argv) <= 1:
         print rickr_note
         print help_string
         return 0
      if '-help' in sys.argv:
         if '-changelog' in sys.argv:
            print change_string
         print rickr_note
         print help_string
         return 0
      if '-changelog' in sys.argv:
         print change_string
         return 0
      if '-apply_censor' in sys.argv:
         #apply the censor file, cutting time points out of a timeseries
         self.apply_censor(sys.argv[2], sys.argv[3], sys.argv[4])
         return 0

      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts
      if not uopts: return 1   #error
      
      for opt in uopts.olist:
         if opt.name == '-anat':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.anat = val
            if self.anat[-1] == '.':
               self.anat = self.anat[0:-1]
         elif opt.name == '-epi':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.epi = val
            if self.epi[-1] == '.':
               self.epi = self.epi[0:-1]
         elif opt.name == '-prefix':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.prefix = val
         elif opt.name == '-aseg':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.aseg = val
         elif opt.name == '-rvt':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.rvt = val
         elif opt.name == '-despike':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val == 'on':
               self.despike = True
            elif val == 'off':
               self.despike = False
            else:
               print "ERROR: %s following -despike is not on or off" % val
               return 1
         elif opt.name == '-tsnr':
            self.tsnr = True
         elif opt.name == '-snr':
            val,err = uopts.get_string_list("", opt=opt)
            if err: return 1
            self.noise = val[0]
            self.channels = val[1]
         elif opt.name == '-outcensor':
            self.outcensor = True
         elif opt.name == '-tlrc':
            self.tlrc = True
         elif opt.name == '-includebrain':
            self.includebrain = True
         elif opt.name == '-localnorm':
            self.localnorm = True
         elif opt.name == '-globalnorm':
            self.globalnorm = True
         elif opt.name == '-modenorm':
            self.modenorm = True
         elif opt.name == '-uniformize':
            self.uniformize = True
         elif opt.name == '-normval':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.normval = val
         elif opt.name == '-wmsize':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.wmsize = val
         elif opt.name == '-fraclimit':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.fraclimit = val
         elif opt.name == '-motlimit':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.motlimit = val
         elif opt.name == '-venterode':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.venterode = val
         elif opt.name == '-episize':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.episize = val
         elif opt.name == '-alignbase':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.alignbase = val
         elif opt.name == '-wmerode':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.wmerode = val
         elif opt.name == '-smoothrad':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.smoothrad = val
         elif opt.name == '-polort':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.polort = val
         elif opt.name == '-trcut':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.trcut = val
         elif opt.name == '-dest':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.dest = val
         elif opt.name == '-align':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val == 'on':
               self.align = True
            elif val == 'off':
               self.align = False
            else:
               print "ERROR: %s following -align is not on or off" % val
               return 1
         elif opt.name == '-anat_has_skull':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val == 'yes':
               self.ss = True
            elif val == 'no':
               self.ss = False
            else:
               print "ERROR: %s following -anat_has_skull is not yes or no" % val
               return 1
         elif opt.name == '-corrmap':
            self.corrmap = True
         elif opt.name == '-exec':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val == 'on':
               self.execute = True
            elif val == 'off':
               self.execute = False
            else:
               print "ERROR: %s following -execute is not on or off" % val
               return 1
         elif opt.name == '-tlrclast':
            self.tlrclast = True
         elif opt.name == '-smooth':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val == 'on':
               self.smoothepi = True
            elif val == 'off':
               self.smoothepi = False
            else:
               print "ERROR: %s following -smooth is not on or off" % val
               return 1
         elif opt.name == '-smoothtogether':
            self.smoothtogether = True
         elif opt.name == '-smoothfirst':
            self.smoothfirst = True
         elif opt.name == '-globalwm':
            self.globalwm = True
         elif opt.name == '-corrmapt':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.corrmapt = val
         elif opt.name == '-script':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            self.script = val
         elif opt.name == '-regressor':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val[-1] == '.':
               val = val[0:-1]
            self.regressors.append(val)
         elif opt.name == '-localregmask':
            val,err = uopts.get_string_list('', opt=opt)
            if err: return 1
            if val[0][-1] == '.':
               val[0] = val[0][0:-1]
            self.regmasks.append(val)
         elif opt.name == '-globalregmask':
            val,err = uopts.get_string_opt('', opt=opt)
            if err: return 1
            if val[-1] == '.':
               val = val[0:-1]
            self.regmasks.append(val)
         elif opt.name == '-bandpass':
            self.bandpass = True
         elif opt.name == '-bpassregs':
            self.bpassregs = True
         elif opt.name == '-setbands':
            val,err = uopts.get_string_list("", opt=opt)
            if err: return 1
            self.bandl = val[0]
            self.bandh = val[1]
         elif opt.name == '-epi2anat':
            self.epi2anat = True
         elif opt.name == '-dvarscensor':
            self.dvarscensor = True
         elif opt.name == '-fdlimit':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.fdlimit = val
         elif opt.name == '-sfdlimit':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.sfdlimit = val
         elif opt.name == '-dvarslimit':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.dvarslimit = val
         elif opt.name == '-censorleft':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.censorleft = val
         elif opt.name == '-censorright':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.censorright = val
         elif opt.name == '-scensorleft':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.scensorleft = val
         elif opt.name == '-scensorright':
            val,err = uopts.get_string_opt("", opt=opt)
            if err: return 1
            self.scensorright = val
         elif opt.name == '-censorunion':
            self.censorunion = True
         elif opt.name == '-keepuncensored':
            self.keepuncensored = True
         elif opt.name == '-dreg':
            self.dreg = True
         
      #make the destination dir match the prefix if it wasn't explicitly named
      if self.dest == "rest_proc":
         self.dest = self.prefix

      #make sure conflicting arguments aren't chosen
      if self.globalnorm and self.modenorm:
         print "ERROR: Please choose either -globalnorm or -modenorm, not both."
         sys.exit(1)
      if self.tlrc and self.tlrclast:
         print "ERROR: Please choose either -tlrc or -tlrclast, not both."
         sys.exit(1)
      if self.outcensor and self.dvarscensor:
         print "ERROR: Please choose either -outcensor or -dvarscensor"
         sys.exit(1)
      if self.tlrc and self.alignbase is not None:
         print "ERROR: Please choose either -tlrc or -alignbase, not both."
         sys.exit(1)
      if self.epi2anat and self.alignbase is not None:
         print "ERROR: Please choose either -epi2anat or -alignbase, not both."
         sys.exit(1)
      if self.epi2anat and self.alignbase is not None:
         print "ERROR: Please choose either -epi2anat or -alignbase, not both."
         sys.exit(1)
      if self.smoothfirst and not self.smoothepi:
         print "ERROR: -smoothfirst and -nosmooth don't make sense together"
         sys.exit(1)
      if self.bpassregs and not self.bandpass:
         print "ERROR: -bpassregs selected without -bandpass"
         sys.exit(1)
      if not self.align and (self.outcensor or self.dvarscensor):
         print "ERROR: -align off is not compatible with -outcensor or -dvarscensor"
         sys.exit(1)
         
      return




   def despike_epi(self, curEPI):
      #despike curEPI and return the name of the despiked timeseries
      #ignore the timepoints that will be cut off
      self.info("Despike EPI")
      epip,epis = remove_suffix(curEPI)
      nextEPI = epip + '.despike'
      cmd = "3dDespike -nomask -prefix %s -ignore %s %s" % (nextEPI, self.trcut, curEPI)
      self.write_execute(cmd)
      return nextEPI + epis

   def normalize_epi(self, curEPI, maskBRAIN):
      #normalize the epi timeseries, only inside the brain
      if not (self.localnorm or self.globalnorm or self.modenorm):
         return curEPI
      if self.localnorm:
         self.info("Normalize based on voxel mean")
      if self.modenorm:
         self.info("Normalize based on global mode")
      if self.globalnorm:
         self.info("Normalize based on global mean")
      epip,epis = remove_suffix(curEPI)
      cmd = "3dTstat -mean -prefix %s.mean %s" % (epip, curEPI)
      self.write_execute(cmd)
      if self.globalnorm:
         #normalize based on global mean
         cmd = "3dmaskave -mask %s -q %s.mean%s > totalmean.1D" % (maskBRAIN, epip, epis)
         self.write_execute(cmd)
         cmd = "3dcalc -a %s -b %s -expr \"(a / `1dcat totalmean.1D\'[0]\'`) * %s * b\" -prefix %s.norm" % \
            (curEPI, maskBRAIN, self.normval, epip)
         self.write_execute(cmd)
      elif self.modenorm:
         #normalize based on global mode
         cmd = "3dhistog -nbins 100 -doall -omit 0 -mask %s %s > %s.hist.1D" % (maskBRAIN, curEPI, epip)
         self.write_execute(cmd)
         cmd = "1dTsort -dec -col 1 %s.hist.1D > %s.hist.sort.1D" % (epip, epip)
         self.write_execute(cmd)
         cmd = "3dcalc -a %s -b %s -expr \"(a / `1dcat %s.hist.sort.1D\'[0]{0}\'`) * %s * b\" -prefix %s.norm" % \
            (curEPI, maskBRAIN, epip, self.normval, epip)
         self.write_execute(cmd)
      elif self.localnorm:
         #normalize based on individual voxel means
         cmd = "3dcalc -a %s -b %s.mean%s -c %s -expr \'(a / b) * %s * c\' -prefix %s.norm" % \
            (curEPI, epip, epis, maskBRAIN, self.normval, epip)
         self.write_execute(cmd)

      return "%s.norm%s" % (epip, epis)

   def convert_to_float(self, curEPI):
      #convert EPI to float to avoid scaling misfit errors
      self.info("Convert to float to avoid scaling misfit errors")
      epip,epis = remove_suffix(curEPI)
      nextEPI = epip + '.float'
      cmd = "3dcalc -a %s -expr 'a' -prefix %s -float" % (curEPI, nextEPI)
      self.write_execute(cmd)
      return nextEPI + epis
         
   def uniformize_anat(self, curANAT):
      #uniformize anatomy
      if not self.uniformize:
         return curANAT
      self.info("Uniformize anatomy")
      anatp,anats = remove_suffix(curANAT)
      nextANAT = anatp + '.uniform'
      cmd = "3dUniformize -anat %s -prefix %s" % (curANAT, nextANAT)
      self.write_execute(cmd)
      return nextANAT + anats

   def align_etc(self, curEPI, curANAT, curASEG, curALIGN, curREGMASKS):
      if not self.align:
         return curEPI,curANAT,None,curASEG,None,curREGMASKS 
      if self.tlrc:
         #tlrc, tshift, and register
         return self.tlrc_all(curEPI,curANAT,curASEG,curREGMASKS)
      else:
         #or tshift, register, and align
         return self.shift_reg_align(curEPI,curANAT,curASEG,curALIGN,curREGMASKS)

   def tlrc_all(self, curEPI, curANAT, curASEG, curREGMASKS):
      #tlrc the anatomical, then align epi to anat
      #also tshift and register at the same time, minimizing interpolation
      #return the names of the motion params and the aligned EPI
      #also transform the aseg file to tlrc space
      self.info("Talairach all input datasets")
      if self.ss:
         cmd = "@auto_tlrc -base TT_N27+tlrc -input %s" % curANAT
      else:
         cmd = "@auto_tlrc -base TT_N27+tlrc -no_ss -input %s" % curANAT
      self.write_execute(cmd)
      epip,epis = remove_suffix(curEPI)
      anatp,anats = remove_suffix(curANAT)
      anatt = anatp + "+tlrc"
      epit = epip + "_tlrc_al+tlrc"
      if self.ss:
         cmd = "align_epi_anat.py -epi %s -anat %s -epi_base 0 -epi2anat -tlrc_apar %s -master_tlrc %s" \
            % (curEPI, curANAT, anatt, self.episize)
      else:
         cmd = "align_epi_anat.py -epi %s -anat %s -anat_has_skull no -epi_base 0 -epi2anat -tlrc_apar %s -master_tlrc %s" \
            % (curEPI, curANAT, anatt, self.episize)
      self.write_execute(cmd)
      #tlrc the aseg file, assuming it is aligned with the original anat
      if curASEG != None:
         asegp,asegs = remove_suffix(curASEG)
         asegt = asegp + "+tlrc"
         cmd = "@auto_tlrc -onewarp -apar %s -rmode NN -input %s " % (anatt, curASEG)
         self.write_execute(cmd)
      else:
         asegt = None
      for i in range(len(curREGMASKS)):
         #talairach any user supplied regressor masks
         if isinstance(curREGMASKS[i], list):
            #local regmask
            regmask = curREGMASKS[i][0]
            regmaskp,regmasks = remove_suffix(regmask)
            regmaskt = regmaskp + "+tlrc"
            cmd = "@auto_tlrc -onewarp -apar %s -rmode NN -input %s " % (anatt, regmask)
            self.write_execute(cmd)
            curREGMASKS[i] = [regmaskt, curREGMASKS[i][1]]
         else:
            #globalregmask
            regmask = curREGMASKS[i]
            regmaskp,regmasks = remove_suffix(regmask)
            regmaskt = regmaskp + "+tlrc"
            cmd = "@auto_tlrc -onewarp -apar %s -rmode NN -input %s " % (anatt, regmask)
            self.write_execute(cmd)
            curREGMASKS[i] = regmaskt
      if self.tshift:
         motparams = epip + "_tsh_vr_motion.1D"
      else:
         motparams = epip + "_vr_motion.1D"    
      return epit,anatt,motparams, asegt, epip + "_al_tlrc_mat.aff12.1D",curREGMASKS
      
   def tlrc_anat(self, curANAT):
      cmd = "@auto_tlrc -base TT_N27+tlrc -no_ss -input %s" % curANAT
      self.write_execute(cmd)
      anatp,anats = remove_suffix(curANAT)
      anatt = anatp + "+tlrc"
      return anatt

   def shift_reg_align(self, curEPI, curANAT, curASEG, curALIGN, curREGMASKS):
      #tshift and register epi 
      #also align anat to epi
      #also align the aseg file
      epip,epis = remove_suffix(curEPI)
      anatp,anats = remove_suffix(curANAT)
      if self.ss:
         has_skull = "yes"
      else:
         has_skull = "no"
      if self.epi2anat:
         #align epi 2 anat
         self.info("Align EPI to ANAT")
         cmd = "align_epi_anat.py -anat %s -epi %s -suffix _al -epi_base 0 -epi2anat -save_vr -volreg on -anat_has_skull %s" % \
            (curANAT, curEPI, has_skull)
         self.write_execute(cmd)
         #no change to aseg because it is already aligned with anat
         aseg_al = curASEG
         epi_al = epip + "_al" + epis
         anat_al = curANAT
         xform = epip + "_al_tlrc_mat.aff12.1D"
      else:
         #align anat 2 epi
         if curALIGN is not None:
            self.info("Align EPI and ANAT to a common EPI base dataset")
            cmd = "align_epi_anat.py -anat %s -epi %s\'[0]\' -suffix _al -epi_base 0 -save_vr -volreg on -child_epi %s -anat_has_skull %s" % \
               (curANAT, curALIGN, curEPI, has_skull)
         else:
            self.info("Align ANAT to EPI")
            cmd = "align_epi_anat.py -anat %s -epi %s -suffix _al -epi_base 0 -save_vr -volreg on -anat_has_skull %s" % \
               (curANAT, curEPI, has_skull)
         self.write_execute(cmd)
         #align the aseg file, assuming it started in alignment with the anat
         if curASEG != None:
            asegp,asegs = remove_suffix(curASEG)
            cmd = "3dAllineate -final NN -1Dmatrix_apply %s_al_mat.aff12.1D -prefix %s_al %s" % (anatp, \
               asegp, curASEG)
            self.write_execute(cmd)
            aseg_al = asegp + "_al" + asegs
         else:
            aseg_al = None
         for i in range(len(curREGMASKS)):
            #align any user supplied regressor masks
            if isinstance(curREGMASKS[i], list):
               #local regmask
               regmask = curREGMASKS[i][0]
               regmaskp,regmasks = remove_suffix(regmask)
               cmd = "3dAllineate -final NN -1Dmatrix_apply %s_al_mat.aff12.1D -prefix %s_al %s" % (anatp, \
                  regmaskp, regmask)
               self.write_execute(cmd)
               curREGMASKS[i] = [regmaskp + "_al" + regmasks, curREGMASKS[i][1]]
            else:
               #globalregmask
               regmask = curREGMASKS[i]
               regmaskp,regmasks = remove_suffix(regmask)
               cmd = "3dAllineate -final NN -1Dmatrix_apply %s_al_mat.aff12.1D -prefix %s_al %s" % (anatp, \
                  regmaskp, regmask)
               self.write_execute(cmd)
               curREGMASKS[i] = regmaskp + "_al" + regmasks
         if self.tshift:
            epi_al = epip + "_tsh_vr" + epis
         else:
            epi_al = epip + "_vr" + epis
         anat_al = anatp + "_al" + anats
         xform = None

      if self.tshift:
         mot = epip + "_tsh_vr_motion.1D"
      else:
         mot = epip + "_vr_motion.1D"
      return epi_al, anat_al, mot, aseg_al, xform, curREGMASKS

   def tcat(self, curEPI, curMOTION, curRVT):
      #remove first self.trcut time points from EPI, motion params, and RVT
      self.info("Remove first TRs from EPI, motion params, and any user supplied regressors")
      nextEPI = self.tcat_4d(curEPI)
      nextMOTION = None
      if curMOTION != None:
         #motion file w/o .1D
         motp = curMOTION[:-3]
         nextMOTION = motp + ".tcat.1D"
         cmd = "1dcat %s\'{%s..$}\' > %s" % (curMOTION, self.trcut, nextMOTION)
         self.write_execute(cmd)

      #since there might not be an RVT file...
      nextRVT = None
      if curRVT != None:
         rvtp = curRVT[:-3]
         nextRVT = rvtp + ".tcat.1D"
         cmd = "1dcat %s\'[0..12]{%s..$}\' > %s" % (curRVT, self.trcut, nextRVT)
         self.write_execute(cmd)

      #catenate the user provided regressors
      for i in range(len(self.regressors)):
         if self.regressors[i] == None:
            continue
         elif self.regressors[i][-3:] == ".1D":
            self.regressors[i] = self.tcat_1d(self.regressors[i])
         elif self.regressors[i][-5:] == "+orig" or self.regressors[i][-5:] == "+tlrc" or self.regressors[i][-5:] == "+lcpc":
            self.regressors[i] = self.tcat_4d(self.regressors[i])
         else:
            print "ERROR: %s is not a .1D or a .BRIK file" % regs[i]


      return nextEPI, nextMOTION, nextRVT

   def tcat_1d(self, cur1D):
      cur1dpre = cur1D[:-3]
      next1d = cur1dpre + ".tcat.1D"
      cmd = "1dcat %s\'{%s..$}\' > %s" % (cur1D, self.trcut, next1d)
      self.write_execute(cmd)
      return next1d
   def tcat_4d(self, cur4D):
      pre,suf = remove_suffix(cur4D)
      nextpre = pre + '.tcat'
      cmd = "3dTcat -prefix %s %s\'[%s..$]\'" % (nextpre, cur4D, self.trcut)
      self.write_execute(cmd)
      return nextpre + suf

   def ext_wm(self, curEPI, curASEG):
      #prep the WM mask using the selected erosion
      #extract the local WM regressors using the selected sphere size
      if curASEG == None:
         return None
      self.info("Prepare white matter regressor mask")
      asegp,asegs = remove_suffix(curASEG)
      cmd = "3dcalc -a %s -prefix mask.WM -expr \'equals(a,2)+equals(a,7)+equals(a,41)+equals(a,46)+equals(a,251)+equals(a,252)+equals(a,253)+equals(a,254)+equals(a,255)+equals(a,16)\'" % curASEG
      self.write_execute(cmd)
      cmd = "3dresample -master %s -rmode NN -inset mask.WM%s -prefix mask.WM.resample" % (curEPI,asegs)
      self.write_execute(cmd)
      cmd = "3dcalc -a mask.WM.resample%s -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k -expr \'a*(step(step(b) + step(c) + step(d) + step(e) + step(f) + step(g) - %d))\' -prefix mask.WM.resample.erode" % (asegs,(6 - int(self.wmerode)))
      self.write_execute(cmd)
      if self.globalwm:
         return "mask.WM.resample.erode%s" % asegs
      else:
         return ["mask.WM.resample.erode%s" % asegs, self.wmsize]
   
   def resample_regmasks(self, curEPI, curREGMASKS):
      if len(filter(None, curREGMASKS)) == 0:
         #if there's nothing to do, return
         return curREGMASKS
      self.info("Resample regressor masks")
      for i in range(len(curREGMASKS)):
         if isinstance(curREGMASKS[i], list):
            #local regmask
            regmask = curREGMASKS[i][0]
            regmaskp,regmasks = remove_suffix(regmask)
            cmd = "3dresample -master %s -rmode NN -inset %s -prefix %s.resample" % (curEPI,regmask,regmaskp)
            self.write_execute(cmd)
            curREGMASKS[i] = ["%s.resample%s" % (regmaskp, regmasks), curREGMASKS[i][1]]
         else:
            #globalregmask
            regmask = curREGMASKS[i]
            regmaskp,regmasks = remove_suffix(regmask)
            cmd = "3dresample -master %s -rmode NN -inset %s -prefix %s.resample" % (curEPI,regmask,regmaskp)
            self.write_execute(cmd)
            curREGMASKS[i] = "%s.resample%s" % (regmaskp, regmasks)
      return curREGMASKS
   def ext_mask_regs(self, curEPI, curREGLIST, curREGMASKS):
      self.info("Extract regressors from masks")
      for i in range(len(curREGMASKS)):
         if curREGMASKS[i] == None:
            continue
         if isinstance(curREGMASKS[i], list):
            #local regmask
            regmask = curREGMASKS[i][0]
            regmaskp,regmasks = remove_suffix(regmask)
            cmd = "3dLocalstat -prefix %s.reg -nbhd \'SPHERE(%s)\' -stat mean -mask %s -use_nonmask %s" % \
               (regmaskp,curREGMASKS[i][1], regmask, curEPI)
            self.write_execute(cmd)
            curREGLIST.append("%s.reg%s" % (regmaskp, regmasks))
         else:
            #globalregmask
            regmask = curREGMASKS[i]
            regmaskp,regmasks = remove_suffix(regmask)
            cmd = "3dmaskave -q -mask %s %s > %s.reg.1D" % (regmask, curEPI, regmaskp)
            self.write_execute(cmd)
            curREGLIST.append("%s.reg.1D" % regmaskp)
      return curREGLIST
      
      
   def ext_vent(self, curEPI, curASEG):
      #prep the ventricle mask using the selected erosion
      #extract the ventricle signal regressor
      if curASEG == None:
         return None
      self.info("Extract ventricle regressor")
      asegp,asegs = remove_suffix(curASEG)
      cmd = "3dcalc -a %s -prefix mask.vent -expr \'equals(a,4)+equals(a,43)\'" % curASEG
      self.write_execute(cmd)
      cmd = "3dresample -master %s -rmode NN -inset mask.vent%s -prefix mask.vent.resample" % (curEPI, asegs)
      self.write_execute(cmd)
      cmd = "3dcalc -a mask.vent.resample%s -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k -expr \'a*(step(step(b) + step(c) + step(d) + step(e) + step(f) + step(g) - %d))\' -prefix mask.vent.resample.erode" % (asegs, (6 - int(self.venterode)))
      self.write_execute(cmd)
      return "mask.vent.resample.erode%s" % asegs

   def ext_brain_mask(self, curANAT, curEPI):
      self.info("Extract brain mask")
      anatp,anats = remove_suffix(curANAT)
      cmd = "3dAutomask -prefix mask.brain %s" % curANAT
      self.write_execute(cmd)
      cmd = "3dresample -master %s -rmode NN -inset mask.brain%s -prefix mask.brain.resample" % \
         (curEPI, anats)
      self.write_execute(cmd)
      return "mask.brain.resample%s" % anats

   def prep_blur(self, maskWM, maskVENT, curANAT, curEPI, maskBRAIN):
      #prep the blurring mask with grey matter labeled 1 and non-grey matter labeled 2
      if maskWM == None:
         #if no aseg file is given, just blur everything
         return maskBRAIN, None
      self.info("Prepare blurring mask with GM=1, NG=2")
      anatp,anats = remove_suffix(curANAT)
      cmd = "3dcalc -a %s -b %s -c %s -prefix mask.grey.nongrey -expr \'a + b + c\'" % \
         (maskBRAIN, maskWM, maskVENT)
      self.write_execute(cmd)
      cmd = "3dcalc -a mask.grey.nongrey%s -expr 'equals(a, 1)' -prefix mask.grey" % anats
      self.write_execute(cmd)
      return "mask.grey.nongrey" + anats, "mask.grey" + anats
      
   def differentiate_all(self, regs):
      #add the first derivatives as additional regressors
      if not self.dreg:
         return regs
      self.info("Differentiate regressors")
      newregs = list(regs)
      for i in range(len(regs)):
         if regs[i] == None:
            continue
         elif regs[i][-3:] == ".1D":
            newregs.append(self.dif_1D(regs[i]))
         elif regs[i][-5:] == "+orig" or regs[i][-5:] == "+tlrc" or regs[i][-5:] == "+lcpc":
            newregs.append(self.dif_4D(regs[i]))
         else:
            print "ERROR: %s is not a .1D or a .BRIK file" % regs[i]
      #regs = newregs
      return newregs

   def dif_1D(self, cur1D):
      #differentiate the given 1D file (simple backwards differences)
      cur1Dp = split_1D(cur1D)
      cmd = "1d_tool.py -derivative -infile %s -write %s.backdif.1D" % (cur1D, cur1Dp)
      self.write_execute(cmd)
      return "%s.backdif.1D" % cur1Dp

   def dif_4D(self, cur4D):
      #differentiate the given 4D file (simple backwards differences)
      epip,epis = remove_suffix(cur4D)
      cmd = "3dcalc -a %s -b \'a[0,0,0,-1]\' -expr \'a - b\' -prefix %s.backdif" % (cur4D, epip)
      self.write_execute(cmd)
      return "%s.backdif%s" % (epip, epis)

   def detrend_all(self, regs):
      if len(filter(None, regs)) == 0 or self.polort == "0":
         #if there's nothing to do, return
         return
      #detrend all regressors, including those supplied by the user
      self.info("Detrend regressors")
      for i in range(len(regs)):
         if regs[i] == None:
            continue
         elif regs[i][-3:] == ".1D":
            regs[i] = self.det_1d(regs[i])
         elif regs[i][-5:] == "+orig" or regs[i][-5:] == "+tlrc" or regs[i][-5:] == "+lcpc":
            regs[i] = self.det_4d(regs[i])
         else:
            print "ERROR: %s is not a .1D or a .BRIK file" % regs[i]

   def det_1d(self, cur1D):
      if cur1D == None:
         return None
      if self.polort == "0":
         return cur1D
      cur1Dp = split_1D(cur1D)
      next1D = cur1Dp + ".detrend.1D"
      cmd = "3dDetrend -DAFNI_1D_TRANOUT=YES -normalize -prefix %s -polort %s %s\\'" % \
         (next1D, self.polort, cur1D)
      self.write_execute(cmd)
      return next1D

   def det_4d(self, cur4D):
      if cur4D == None:
         return None
      if self.polort == "0":
         return cur4D
      cur4Dp,cur4Ds = remove_suffix(cur4D)
      next4D = cur4Dp + ".detrend" + cur4Ds
      cmd = "3dDetrend -normalize -prefix %s.detrend -polort %s %s" % (cur4Dp, self.polort, cur4D)
      self.write_execute(cmd)
      return next4D

   def regress(self, curEPI, regs):
      self.info("Regress out nuisance signals")
      epip,epis = remove_suffix(curEPI)
      regstr = ""
      for r in regs:
         regstr = regstr + " " + nstr(r)
      cmd = "3dTfitter -polort %s -RHS %s -LHS %s -prefix %s.regcoefs -fitts %s.fitts" % (self.polort, nstr(curEPI), \
         regstr, epip, epip)
      self.write_execute(cmd)
      
      cmd = "3dcalc -a %s -b %s.fitts%s -expr \'a - b\' -prefix %s.clean -float" % (curEPI, epip, epis, epip)
      self.write_execute(cmd)
      return epip + ".clean" + epis


   def smooth(self, curEPI, maskBLUR):
      self.info("Smooth timeseries")
      epip,epis = remove_suffix(curEPI)
      cmd = "3dBlurInMask -input %s -FWHM %s -Mmask %s -prefix %s.blur" % (curEPI, self.smoothrad, maskBLUR, \
         epip)
      self.write_execute(cmd)
      return epip + ".blur" + epis

   def copy_result_1D(self, dset, dest):
      if dset == None:
         return
      cmd = "cp %s %s" % (dset, dest)
      self.write_execute(cmd)

   def copy_result_BRIK(self, dset, dest, anatt, rmode):
      #copy a result file that is in head/brik format
      #the result may not exist, so maybe do nothing
      if dset == None:
         return
      #talairach dset if tlrclast was chosen
      if self.tlrclast:
         dsetp,dsets = remove_suffix(dset)
         cmd = "@auto_tlrc -onewarp -apar %s -rmode %s -dxyz %s -input %s" % (anatt, rmode, self.episize, dset)
         self.write_execute(cmd)
         dset = dsetp + "+tlrc"
      cmd = "3dcopy %s %s" % (dset, dest)
      self.write_execute(cmd)
   def just_copy_result_BRIK(self, dset, dest):
      #just copy the result, no chance of talairaching (to handle the anat)
      if dset == None:
         return
      cmd = "3dcopy %s %s" % (dset, dest)
      self.write_execute(cmd)
      
   def create_out_censor(self, origEPI, curMOTION):
      #create a censor file based on ourliers in origEPI and excess motion in curMOTION
      #get outlier fractions
      self.info("Create outlier and RMS motion based censor file")
      epip,epis = remove_suffix(origEPI)
      cmd = "3dAutomask -prefix %s.brainmask %s" % (epip, origEPI)
      self.write_execute(cmd)
      cmd = "3dToutcount -mask %s.brainmask%s -fraction -polort %s -legendre %s\'[%s..$]\' > %s.outfracs.1D" \
         % (epip, epis, self.polort, origEPI, self.trcut, epip)
      self.write_execute(cmd)
      #create temporal mask based on outlier fraction (including neighboring timepoints)
      curOUTCEN = self.mask_moderate_vals("%s.outfracs.1D" % epip, self.fraclimit, self.censorleft, self.censorright)

      #create temporal mask  based on excessive motion
      cmd = "1d_tool.py -infile %s -set_tr %s -censor_motion %s motion" % (curMOTION, self.tr, self.motlimit)
      self.write_execute(cmd)
      #include neighboring timepoints in the mask
      curMOTCEN = self.mask_neighbors("motion_censor.1D", -int(self.censorleft))
      curMOTCEN = self.mask_neighbors(curMOTCEN, int(self.censorright))

      #combine the two censor files
      #if either file contains a 0 at a point, censor it -> union of bad time points
      cmd = "1deval -a %s -b %s -expr \'and(a, b)\' > outCensorUnion.1D" % (curOUTCEN, curMOTCEN)
      self.write_execute(cmd)
      #if both files contains 0's at a point, censor it -> intersection of bad time points
      cmd = "1deval -a %s -b %s -expr \'or(a, b)\' > outCensorIntersection.1D" % (curOUTCEN, curMOTCEN)
      self.write_execute(cmd)
      if self.censorunion:
         return "outCensorUnion.1D"
      else:
         return "outCensorIntersection.1D"

   def create_power_censor(self, curMOTION, curEPI, maskBRAIN):
      #Power as in the author...
      self.info("Create FD and DVARS based censor file")
      fdCEN = self.create_fd_censor(curMOTION, curEPI)
      dvarsCEN = self.create_dvars_censor(curEPI, maskBRAIN)
      #if either file contains a 0 at a point, censor it -> union of bad time points
      cmd = "1deval -a %s -b %s -expr \'and(a, b)\' > powerCensorUnion.1D" % (fdCEN, dvarsCEN)
      self.write_execute(cmd)
      #if both files contains 0's at a point, censor it -> intersection of bad time points
      cmd = "1deval -a %s -b %s -expr \'or(a, b)\' > powerCensorIntersection.1D" % (fdCEN, dvarsCEN)
      self.write_execute(cmd)
      if self.censorunion:
         return "powerCensorUnion.1D"
      else:
         return "powerCensorIntersection.1D"

   def create_fd_censor(self, curMOTION, curEPI):
      #create a censor file based on the FD measure from Power et. al Neuroimage 2012
      self.info("create FD censor")
      cur1Dp = split_1D(curMOTION)
      cmd = "1d_tool.py -infile %s -derivative -write %s.deltamotion.1D" % (curMOTION, cur1Dp)
      self.write_execute(cmd)
      cur1D = "%s.deltamotion.1D" % cur1Dp
      cmd = "1deval -a %s\'[0]\' -b %s\'[1]\' -c %s\'[2]\' -d " % (cur1D, cur1D, cur1D) + \
         "%s\'[3]\' -e %s\'[4]\' -f %s\'[5]\' -expr " % (cur1D, cur1D, cur1D) + \
         "\'100*sind(abs(a)/2) + 100*sind(abs(b)/2) + " + \
         "100*sind(abs(c)/2) + abs(d) + abs(e) + abs(f)\' > %s.deltamotion.FD.1D" % (cur1Dp)
      self.write_execute(cmd)
      curFDCEN = self.mask_moderate_vals("%s.deltamotion.FD.1D" % cur1Dp, self.fdlimit, self.censorleft, self.censorright)
      return curFDCEN

   def create_dvars_censor(self, curEPI, maskBRAIN):
      #create a censor file based on the dvars measure from Power et. al Neuroimage 2012
      self.info("create DVARS censor")
      epip,epis = remove_suffix(curEPI)
      cmd = "3dcalc -a %s -b \'a[0,0,0,-1]\' -expr \'(a - b)^2\' -prefix %s.backdif2" % (curEPI, epip)
      self.write_execute(cmd)
      cmd = "3dmaskave -mask %s -quiet %s.backdif2%s > %s.backdif2.avg.1D" % (maskBRAIN, epip, epis, epip)
      self.write_execute(cmd)
      cmd = "1deval -a %s.backdif2.avg.1D -expr 'sqrt(a)'  > %s.backdif2.avg.dvars.1D" % (epip, epip)
      self.write_execute(cmd)
      curCEN = self.mask_moderate_vals("%s.backdif2.avg.dvars.1D" % epip, self.dvarslimit, self.censorleft, self.censorright)
      cmd = "3dTstat -mean -stdev -prefix %s.backdif2.avg.dvars.stats.1D %s.backdif2.avg.dvars.1D\\\'" % \
         (epip, epip)
      self.write_execute(cmd)
      return curCEN

   def mask_moderate_vals(self, cur1D, thresh, censorleft, censorright):
      #make a mask of time points in cur1D that are within [-thresh,thresh]
      cur1Dp = split_1D(cur1D)
      cmd = "1d_tool.py -infile %s -extreme_mask -1 %s -write %s.extreme%s.1D" % (cur1D, thresh, cur1Dp, thresh)
      self.write_execute(cmd)
      cmd = "1deval -a %s.extreme%s.1D -expr \'not(a)\' > %s.moderate%s.1D" % (cur1Dp, thresh, cur1Dp, thresh)
      self.write_execute(cmd)
      #also mask time points censorleft and censorright points left and right
      cur1D = self.mask_neighbors(cur1Dp + ".moderate%s.1D" % thresh, -int(censorleft))
      cur1D = self.mask_neighbors(cur1D, int(censorright))
      return cur1D

   def mask_neighbors(self, cur1D, stepsremaining):
      cur1Dp = split_1D(cur1D)
      if stepsremaining < 0:
         cmd = "1deval -a %s -b %s\'{1..$,0}\' -expr \'ispositive(a + b - 1)\' > %s.n.1D" % (cur1D, cur1D, cur1Dp)
         self.write_execute(cmd)
         return self.mask_neighbors("%s.n.1D" % cur1Dp, stepsremaining + 1)
      if stepsremaining > 0:
         cmd = "1deval -a %s -b %s\'{0,0..$}\' -expr \'ispositive(a + b - 1)\' > %s.n.1D" % (cur1D, cur1D, cur1Dp)
         self.write_execute(cmd)
         return self.mask_neighbors("%s.n.1D" % cur1Dp, stepsremaining - 1)
      return cur1D

   def compute_snr(self, origEPI, origNOISE,  channels, epiXFORM, curEPI):
      #compute the SNR based on a scan with the RF turned off...
      self.info("Compute SNR")
      epip,epis = remove_suffix(origEPI)
      noisep,noises = remove_suffix(origNOISE)
      cmd = "3dmaskave -sigma -q %s\'[0]\' > %s.noise.1D" % (origNOISE, noisep)
      self.write_execute(cmd)
      
      #correction factors for SNR computation
      corr1 = 1.5263997
      corr8 = 1.4257312
      corr16 = 1.4198559
      corr32 = 1.4170053
      if channels == "1":
         cor = corr1
      elif channels == "8":
         cor = corr8
      elif channels == "16":
         cor = corr16
      elif channels == "32":
         cor = corr32
      else:
         print ("ERROR unknown number of channels: %s" % channels)
         sys.exit(1)
      cmd = "3dcalc -a %s\'[0]\' -expr \"a / (`1dcat %s.noise.1D\'[1]\'` * %s)\" -prefix %s.SNR" \
         % (origEPI, noisep, cor, epip)
      self.write_execute(cmd)
      SNR = "%s.SNR%s" % (epip, epis)

      if epiXFORM is not None:
         mastp,masts = remove_suffix(curEPI)
         cmd = "3dAllineate -cubic -1Dmatrix_apply %s -prefix %s.SNR_al -master %s %s" % (epiXFORM, epip, curEPI, SNR)
         self.write_execute(cmd)
         SNR = "%s.SNR_al%s" % (epip, masts)
      
      #save a histogram of the SNR
      cmd = "3dhistog -min 0 -max 1300 %s > SNRhist.1D" % SNR
      self.write_execute(cmd)
      return SNR
      
   def write_apply_censor(self, curEPI, curCENSOR, prefix):
      #write the command to call this script to apply the censor file, in case -noexec was chosen
      #(in which case the censor file has not been created before, but will be when apply_censor is called)
      if curCENSOR == None:
         return curEPI
      self.info("Apply censor file")
      cmd = "afni_restproc.py -apply_censor %s %s %s" % (curEPI, curCENSOR, prefix)
      self.write_execute(cmd)
      epip,epis = remove_suffix(curEPI)
      return "%s%s" % (prefix, epis)

   def apply_censor(self, curEPI, curCENSOR, prefix):
      #apply the given censor file, cutting TRs out of the EPI timeseries
      selectstr = ''
      i = 0
      f = open(curCENSOR)
      #select time points corresponding to lines without zeros (with ones)
      for line in f:
         if line.find('0') == -1:
            selectstr += str(i) + ','
         i = i + 1
      selectstr = selectstr[0:-1]
      cmd = "3dTcat -prefix %s %s\'[%s]\'" % (prefix, curEPI, selectstr)
      self.write_execute(cmd)

   def compute_tsnr(self, curEPI):
      #compute the tSNR of curEPI as mean(curEPI) / stdev(det(curEPI)) where det(curEPI) is curEPI detrended with
      #polynomial order polort
      if not self.tsnr:
         return None
      self.info("Compute TSNR")
      epip,epis=remove_suffix(curEPI)
      cmd = "3dTstat -mean -prefix %s.tmean %s" % (epip, curEPI)
      self.write_execute(cmd)
      cmd = "3dDetrend -prefix %s.det -polort %s %s" % (epip, self.polort, curEPI)
      self.write_execute(cmd)
      cmd = "3dTstat -stdev -prefix %s.det.stdev %s" % (epip, epip + ".det" + epis)
      self.write_execute(cmd)
      cmd = "3dcalc -a %s.tmean%s -b %s.det.stdev%s -expr \'a / b\' -float -prefix %s.tsnr" % (epip, epis, \
         epip, epis, epip)
      self.write_execute(cmd)
      return epip + ".tsnr" + epis

   def bpass(self, curEPI, regs):
      #do bandpass filtering
      if not self.bandpass:
         return curEPI
      if self.bpassregs:
         self.bpass_all_regs(regs)
      self.info("Bandpass filter EPI")
      epip,epis = remove_suffix(curEPI)
      return self.bpass_4d(curEPI)

   def bpass_all_regs(self, regs):
      if len(filter(None, regs)) == 0:
         #if there's nothing to do, return
         return
      #bandpass filter all regressors, including those supplied by the user
      self.info("Bandpass filter regressors")
      for i in range(len(regs)):
         if regs[i] == None:
            continue
         elif regs[i][-3:] == ".1D":
            regs[i] = self.bpass_1d(regs[i])
         elif regs[i][-5:] == "+orig" or regs[i][-5:] == "+tlrc" or regs[i][-5:] == "+lcpc":
            regs[i] = self.bpass_4d(regs[i])
         else:
            print "ERROR: %s is not a .1D or a .BRIK file" % regs[i]

   def bpass_1d(self, cur1D):
      if cur1D == None:
         return None
      cur1Dp = split_1D(cur1D)
      next1D = cur1Dp + ".bpass.1D"
      cmd = "1dBandpass -nodetrend %s %s %s > %s" % (self.bandl, self.bandh, cur1D, next1D)
      self.write_execute(cmd)
      return next1D

   def bpass_4d(self, cur4D):
      if cur4D == None:
         return None
      cur4Dp,cur4Ds = remove_suffix(cur4D)
      next4D = cur4Dp + ".bpass" + cur4Ds
      cmd = "3dBandpass -nodetrend -prefix %s.bpass %s %s %s" % (cur4Dp, self.bandl, self.bandh, cur4D)
      self.write_execute(cmd)
      return next4D

   def set_polort(self, curEPI):
      #only set self.polort if it is -1 (not set by the user)
      com = shell_com("3dinfo -tr %s" % curEPI, self.oexec, capture=1)
      com.run()
      self.tr = ''.join(com.so)
      com = shell_com("3dinfo -nv %s" % curEPI, self.oexec, capture=1)
      com.run()
      self.trs = ''.join(com.so)
      if self.polort != '-1':
         return
      #default polort is 1 + floor(length / 150)
      self.polort = str(int(1 + float(self.tr) * float(self.trs) / 150))

   def set_tshift(self, curEPI):
      #determine if the input EPI has already been tshifted
      com = shell_com("3dAttribute TAXIS_OFFSETS %s" % curEPI, self.oexec, capture=1)
      com.run()
      if len(com.so):
         self.tshift = True
      else:
         self.tshift = False

   def compute_corrmap(self, curEPI, maskGM):
      #compute average correlation maps using 3dTcorrMap
      if not self.corrmap:
         return None,None,None
      self.info("Compute global functional connectivity")
      epip,epis = remove_suffix(curEPI)
      cmd = "3dTcorrMap -input %s -mask %s -polort -1 -Hist 400 %s.CorHist -mean %s.MeanCorr -Cexpr \'step(r-%s)*r\' %s.MeanCorrGT" \
         % (curEPI, maskGM, epip, epip, self.corrmapt, epip)
      self.write_execute(cmd)
      return epip + '.MeanCorr' + epis, epip + '.MeanCorrGT' + epis, epip + '.CorHist' + epis
   def process_data(self):
      #make sure the result directory doesn't already exist
      if os.path.exists(self.dest):
         print "ERROR: Result directory %s already exists.  Will not overwrite." % self.dest
         sys.exit(1)
      
      #get the cwd to be able to copy files specified by relative paths
      origdir = os.getcwd()

      #make the destination dir and cd into it
      os.makedirs("%s" % self.dest)
      os.chdir(self.dest)
      if self.script != None:
         self.script = open(self.script, "w")
         self.script.write("#!/usr/bin/tcsh\n\n")
         self.script.write("cd tmp\n")


      #make tmp dir for all input and intermediate files
      os.mkdir("tmp")
      os.chdir("tmp")

      #copy files into the temp directory
      curASEG = copy_input(self.aseg, origdir)
      curALIGN = copy_input(self.alignbase, origdir)
      curNOISE = copy_input(self.noise, origdir)
      curRVT = copy_input(self.rvt, origdir)
      for i in range(len(self.regressors)):
         self.regressors[i] = copy_input(self.regressors[i], origdir)
      for i in range(len(self.regmasks)):
         self.regmasks[i] = copy_input(self.regmasks[i], origdir)
      curANAT = copy_input(self.anat, origdir)
      curEPI = copy_input(self.epi, origdir)

      #set polort automatically if it was not chosen by the user
      self.set_polort(curEPI)
      #determine if the input dataset has already been tshifted
      self.set_tshift(curEPI)
      #convert epi to float format to avoid bothersome scaling misfit errors
      curEPI = self.convert_to_float(curEPI)
      #save origional epi name for censoring later 
      origEPI = curEPI
      #despike
      if self.despike:
         curEPI = self.despike_epi(curEPI)
      #uniformize anatomy
      curANAT = self.uniformize_anat(curANAT)

      #align, tshift, tlrc, register, save the xform for transforming the SNR later
      curEPI,curANAT,curMOTION,curASEG,epiXFORM,curREGMASKS = self.align_etc(curEPI, curANAT, curASEG, curALIGN, self.regmasks)
      #tcat
      curEPI,curMOTION,curRVT = self.tcat(curEPI, curMOTION, curRVT)
      cattedMOTION = curMOTION
      #compute tsnr
      curTSNR = self.compute_tsnr(curEPI)
      #extract brain mask
      maskBRAIN = self.ext_brain_mask(curANAT, curEPI)
      #normalize the EPI 
      curEPI = self.normalize_epi(curEPI, maskBRAIN)

      #resample user supplied reg masks to match EPI
      curREGMASKS = self.resample_regmasks(curEPI, curREGMASKS)

      #prep white matter mask
      maskWM = self.ext_wm(curEPI, curASEG)
      curREGMASKS.append(maskWM)
      if isinstance(maskWM, list):
         maskWM = maskWM[0]
      #prep ventricle mask
      maskVENT = self.ext_vent(curEPI, curASEG)
      curREGMASKS.append(maskVENT)
      #prep blurring mask
      maskBLUR,maskGM = self.prep_blur(maskWM, maskVENT, curANAT, curEPI, maskBRAIN)
      #include whole brain regressor, if requested
      if self.includebrain:
         curREGMASKS.append(maskBRAIN)
      #smooth before regression, if desired
      if self.smoothfirst:
         if self.smoothtogether:
            curEPI = self.smooth(curEPI, maskBRAIN)
         else:
            curEPI = self.smooth(curEPI, maskBLUR)
      #extract regressors from masks
      self.regressors = self.ext_mask_regs(curEPI, self.regressors, curREGMASKS)
      self.regressors.append(curMOTION)
      self.regressors.append(curRVT)
      
      #differentiate regressors, if requested
      self.regressors = self.differentiate_all(self.regressors)

      #detrend regressors
      self.detrend_all(self.regressors)

      #do bandpass filtering
      curEPI = self.bpass(curEPI, self.regressors)

      #do regression
      curEPI = self.regress(curEPI, self.regressors)


      #compute SNR
      if self.noise != None:
         SNR = self.compute_snr(origEPI, curNOISE, self.channels, epiXFORM, curEPI)
      else:
         SNR = None
      #create censor file
      if self.dvarscensor:
         curCENSOR = self.create_power_censor(cattedMOTION, curEPI, maskBRAIN)
      elif self.outcensor:
         curCENSOR = self.create_out_censor(origEPI, curMOTION)
      else:
         curCENSOR = None

      #smooth results
      if self.smoothepi and not self.smoothfirst:
         if self.smoothtogether:
            curEPI = self.smooth(curEPI, maskBRAIN)
         else:
            curEPI = self.smooth(curEPI, maskBLUR)

      #remove censored trs
      epip,epis=remove_suffix(curEPI)
      if self.keepuncensored:
         curEPIUNC = curEPI
      else:
         curEPIUNC = None
      curEPI = self.write_apply_censor(curEPI, curCENSOR, "%s.censor" % epip)

      #compute corr map
      curMC,curMCGT,curHIST = self.compute_corrmap(curEPI, maskGM)
      #copy and name results
      #also talairach them if -tlrclast is chosen
      self.info("Copy results, possibly talairaching them")
      if self.tlrclast:
         curANATT = self.tlrc_anat(curANAT)
         self.just_copy_result_BRIK(curANATT, "../%s.anat" % (self.prefix))
      else: 
         curANATT = None
         self.just_copy_result_BRIK(curANAT, "../%s.anat" % (self.prefix))
      
      self.copy_result_1D(curCENSOR, "../%s.censor.1D" % self.prefix)
      self.copy_result_BRIK(curEPI, "../%s.cleanEPI" % (self.prefix), curANATT, "quintic")
      self.copy_result_BRIK(curEPIUNC, "../%s.cleanEPI.uncensored" % (self.prefix), curANATT, "quintic")
      self.copy_result_BRIK(maskVENT, "../%s.mask.vent" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(maskWM, "../%s.mask.wm" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(maskGM, "../%s.mask.gm" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(maskBRAIN, "../%s.mask.brain" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(curTSNR, "../%s.tsnr" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(SNR, "../%s.snr" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(curMC, "../%s.meancorr" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(curMCGT, "../%s.meancorrGT" % (self.prefix), curANATT, "NN")
      self.copy_result_BRIK(curHIST, "../%s.CorHist" % (self.prefix), curANATT, "NN")

      return

   def write_execute(self, cmd):
      if self.script != None:
         self.script.write(cmd + "\n")
      if self.execute:
         shell_com(cmd).run()
   def info(self, msg):
      if self.script != None:
         self.script.write("\n#%s\n" % msg)
      if self.execute:
         shell_com("echo INFO: %s" % msg).run()
         

def copy_input(infile, origdir):
   if infile == None:
      return None
   if isinstance(infile, list):
      #should only be true when processing a localregmask--will have infile and radius
      if infile[0][0] == '/':
         #input is an absolute path
         copy_or_error("%s*" % infile[0])
         return [remove_path_and_convert(infile[0]), infile[1]]
         return [remove_path(infile[0]), infile[1]]
      else:
         #input is a relative path
         copy_or_error("%s/%s*" % (origdir, infile[0]))
         return [remove_path_and_convert(infile[0]), infile[1]]
         return [remove_path(infile[0]), infile[1]]
       
   if infile[0] == '/':
      #input is an absolute path
      copy_or_error("%s*" % infile)
      return remove_path_and_convert(infile)
      return remove_path(infile)
   else:
      #input is a relative path
      copy_or_error("%s/%s*" % (origdir, infile))
      return remove_path_and_convert(infile) 
      return remove_path(infile) 
   print "ERROR: copying input %s.  This should never happen" % infile
   sys.exit(1)

def copy_or_error(infile):
   #copy an input file if it exists, otherwise throw an error
   s = shell_com("cp %s ." % infile, capture=1)
   s.run()
   if s.se:
      print "ERROR: Cannot find input file %s.  Exiting." % infile
      sys.exit(1)

def nstr(s):
   #to convert None to "" conveniently
   if s == None:
      return ""
   return str(s)

def remove_path_and_convert(dset):
   #remove path and convert any .nii or .mgz files to .BRIK format
   infile = dset[dset.rfind('/') + 1::]
   if infile == None:
      return None
   if infile[-4:] == ".mgz":
      inp = infile[:-4]
      cmd = "mri_convert -it mgz -ot nii -i %s -o %s.nii" % (infile, inp)
      shell_com(cmd, capture=0).run()
      #self.write_execute(cmd)
      infile = inp + ".nii"
   if infile[-4:] == ".nii":
      inp = infile[:-4]
      cmd = "3dcalc -a %s -expr \'a\' -prefix %s+orig" % (infile, inp)
      shell_com(cmd, capture=0).run()
      #self.write_execute(cmd)
      infile = inp + "+orig"
   return infile
   

def remove_path(dset):
   return dset[dset.rfind('/') + 1::]

def remove_suffix(dset):
   #remove +ORIG +TLRC or +ACPC (possibly followed by .) from dset
   #also return the suffix
   if dset[-1] == '.':
      return dset[0:-6],dset[-6:]
   return dset[0:-5],dset[-5:]

def split_1D(dset):
   return dset[0:-3]


def main():
   nint = RestInterface()
   if not nint: return 1
   rv = nint.process_options()
   if rv != None: return rv

   rv = nint.process_data()
   if rv != None: return rv

   return
        
if __name__ == '__main__':
   sys.exit(main())

