3dPFM Sample Data README
=========================

This program is written in R. Consequently, to install R and make AFNI compatible with R, please follow the guidelines in http://afni.nimh.nih.gov/sscc/gangc/Rinstall.html

In addition, please install the following libraries with dependencies:

install.packages("abind",dependencies=TRUE)
install.packages("MASS",dependencies=TRUE)
install.packages("lars",dependencies=TRUE)


Introduction
------------
This package contains the datasets for the demo of the 3dPFM function, which implements the Paradigm Free Mapping method described in [1]. This demo includes the preprocessed T2*-weighted Gradient-Echo EPI fMRI dataset  (see below for details about acquisition and proprecessing), as well as some minimal datasets and time series that can be computed with 3dPFM so that you don't have to wait for the end of the computation. The data was acquired from a healthy adult on a 7T Philips Achieva scanner (Best, Netherlands) using a 16-channel head coil (Nova Medical, MA) and corresponds to Subject A in [1] and [2].

Data Acquisition and Preprocessing
----------------------------------

The fMRI data was acquired using a single-shot gradient-echo echo planar imaging (EPI) sequence with 2-mm isotropic resolution, matrix size = 112 x 112, in-plane FOV = 224 x 224 mm, sensitivity encoding (SENSE) factor = 1.5, echo time (TE) = 30 ms, repetition time (TR) = 2 s, and flip angle = 80º. The number of scans in the acquisition was 342. Twenty oblique slices were acquired at approx. 15 degrees to the cantomeatal line above the corpus callosum to cover areas from superior frontal to occipital cortices. Cardiac and respiratory data were recorded using a respiratory belt and a pulse oximeter to allow physiological noise correction of the data with RETROICOR. 

This demo data includes the dataset after basic preprocessing, including:

1) Rigid body registration to correct for motion with 3dVolreg (using -Fourier interpolation, -twopass option, and saving translation and rotation parameters in -1Dfile SETR2RegParam.1D) 
2) Correction for physiological noise using retrospective correction of physiological motion effects (RETROICOR) considering 2 harmonics of the cardiac and respiratory phases [Glover et al., 2000]
3) Detrending using 3dDetrend with sine and cosine waveforms with one cycle over the scan duration and up to fourth-order Legendre polynomials. The mean of voxel time series was restored after detrending.


No spatial smoothing is performed. 

Experimental Paradigm
---------------------

The fMRI run lasted 684 s, and started with an initial rest period of 140 s, followed by two trials of visually cued finger tapping at 140 s and 180 s (trial duration: 4 s) and followed by a second rest period from 184 to 384 s. At 384 s, a message (TAP at will) was projected onto a screen instructing subjects to carry out two additional trials of 4 s finger tapping at a time of their choosing (no visual cue). Thus, the final 300 s of the run duration included a rest period of variable length (in the range of 164 and 224 s) depending on each subject’s performance. During rest periods, subjects were asked to fixate on a cross. The visual instructions were projected from an LCD projector via angled mirrors onto a screen located inside the scanner room, viewed through prism glasses. Subjects were instructed about the paradigm before the scanning session.


Usage
-----
To replicate the results in [2], use the following commands to perform a basic PFM analysis of the data:

# create brain mask with 3dAutomask 
3dAutomask -prefix mask_SETR2.nii.gz SETR2RegRCR.nii.gz

# demean realignment parameters
1d_tool.py -infile SETR2RegParam.1D -demean -write SETR2RegParam_demean.1D

# normalize signal to signal percentage
3dTstat -mean -prefix mean_SETR2RegRCR.nii.gz SETR2RegRCR.nii.gz
3dcalc -a SETR2RegRCR.nii.gz -b mean_SETR2RegRCR.nii.gz -expr '(a-b)/b' -prefix SETR2RegRCRpc.nii.gz

# run 3dPFM
3dPFM -input SETR2RegRCRpc.nii.gz -mask mask_SETR2.nii.gz -algorithm dantzig -criteria bic -LHS SETR2RegParam_demean.1D -hrf SPMG1 -jobs 14 -outALL ds

# This example considers the SPM canonical HRF for the deconvolution and the (demeaned) realignment parameters as LHS regressors. The deconvolution is done with the dantzig selector algorithm where the regularization parameter is set according to the Bayesian Information Criterion (BIC) in each voxel. Furthermore, the program will save all the possible output volumes and use parallel computation using the snow R-package in 14 nodes (i.e. remove this option or change number of jobs to match your needs). Computations will only be performed within non-zero voxels in mask mask_SETR2.nii.gz

# In this case, 3dPFM will creates 3D+time volumes with the results of the deconvolution, as well as the corresponding statistics (see 3dPFM -help for more information). In order to explore the results, we recommend to see movie with the results of the deconvolution as well as to create Activation Time Series that summarize the results in time (see below). 

# To see a movie with the results of 3dPFM, we recommend to proceed as follows. Set the UNDERLAY volume to SETR2RegRCR.nii.gz and, for instance, set the OVERLAY volume to beta_ds_SETR2RegRCR.nii.gz. Open a graph to see the time series. Set the AFNI environment variable AFNI_SLAVE_FUNCTIME = YES, and AFNI_SLAVE_THROLAY=YES This will change the overlay to the same sub-brick index as the functional dataset. Set the Overlay threshold to 0 (or any relevant value if you want to set a threshold in the amplitude of the beta coefficients to be seen in overlay). In the graph time series, press the left/right-arrow keys to move the cursor. Enjoy the movie. 

# In tcsh environment
setenv AFNI_SLAVE_FUNCTIME YES
setenv AFNI_SLAVE_THROLAY YES
# In bash environment
export AFNI_SLAVE_FUNCTIME=YES
export AFNI_SLAVE_THROLAY=YES

afni \
-com "SWITCH_UNDERLAY A SETR2RegRCR.nii.gz" \
-com "SWITCH_OVERLAY A beta_ds_SETR2RegRCRpc.nii.gz" \
-com 'OPEN_WINDOW A.axialimage geom=800x800 mont=5x4:1 ifrac=1' \
-com 'OPEN_WINDOW A.sagittalimage geom=600x600 mont=7x6:1 ifrac=1' \
-com 'SET_PBAR_ALL A.+99 1.0 Spectrum:red_to_blue+gap' \
-com 'SET_THRESHNEW A 0' \
-com 'SET_IJK A 35 67 12' \
-com 'SET_VIEW A.orig' \
-com 'SET_XHAIRS A.SINGLE' \
-com 'OPEN_WINDOW A.axialgraph -keypress=v'


# create Activation Time Series, counting the number of voxels with positive or negative beta coefficients at each time point
3dcalc -a beta_ds_SETR2RegRCRpc.nii.gz -expr 'ispositive(a)' -prefix pos_beta_ds_SETR2RegRCRpc.nii.gz
3dmaskave -q -mask mask_SETR2.nii.gz -sum pos_beta_ds_SETR2RegRCRpc.nii.gz > pos_ATSnum_ds_SETR2RegRCRpc.1D
3dcalc -a beta_ds_SETR2RegRCRpc.nii.gz -expr 'isnegative(a)' -prefix neg_beta_ds_SETR2RegRCRpc.nii.gz
3dmaskave -q -mask mask_SETR2.nii.gz -sum neg_beta_ds_SETR2RegRCRpc.nii.gz > neg_ATSnum_ds_SETR2RegRCRpc.1D
1dplot -sepscl pos_ATSnum_ds_SETR2RegRCRpc.1D neg_ATSnum_ds_SETR2RegRCRpc.1D


See http://afni.nimh.nih.gov/pub/dist/doc/program_help/3dPFM.html for other options.


References
----------
[1] Caballero-Gaudes C, Petridou N, Francis ST, Dryden IL, Gowland PA (2013). Paradigm free mapping with sparse regression automatically detects single-trial functional magnetic resonance imaging blood oxygenation level dependent responses. Hum. Brain Mapp., 34(3): 501–518.
[2] Petridou N, Caballero-Gaudes C, Dryden IL, Francis ST, Gowland PA (2013). Periods of rest in fMRI contain individual spontaneous events which are related to slowly fluctuating spontaneous activity. Hum. Brain Mapp., 34(6): 1319–1329.
[3] Caballero-Gaudes C, Petridou N, Dryden IL, Bai L, Francis ST, Gowland PA (2011). Detection and characterization of single-trial fMRI bold responses: Paradigm free mapping. Hum. Brain Mapp., 32(9): 1400–1418.
[4] Glover GH, Li TQ, Ress D (2000). Image-based method for retrospective correction of physiological motion effects in fMRI: RETROICOR. Magn Reson Med, 44(1): 162–167. 
    
