#!/usr/bin/env AFNI_Batch_R

#Clean up
rm(list = ls())

first.in.path <- function(file) {
  ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
  ff<-ff[lapply(ff,file.exists)==TRUE];
  #cat('Using ', ff[1],'\n');
  return(gsub('//','/',ff[1], fixed=TRUE))
}
source(first.in.path('AFNIio.R'))

#Make sure you set this variable to your own program name.
#Do not include the .R part
ExecName <- '3dMEPFM'

#################################################################################
################### Begin RprogDemo Input functions #############################
#################################################################################

greeting.RprogDemo <- function ()
  return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 3dMEPFM ==============================
          #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          Version 1.0.1, May 19 2022
          Author: Cesar Caballero Gaudes (c.caballero@bcbl.eu)
          Basque Center on Cognition, Brain and Language, Spain
          #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          "
  )

#The help function for 3dMEPFM batch (command line mode)
help.RprogDemo.opts <- function (params, alpha = TRUE,
                                 itspace='   ', adieu=FALSE) {

  intro <-
    '
  Usage: 3dMEPFM [options]
  ------

  Brief summary:
  ==============
  * 3dMEPFM is the equivalent program to 3dPFM for Multiecho fMRI data. This program 
    performs the voxelwise deconvolution of ME-fMRI data to yield time-varying estimates 
    of the changes in the transverse relaxation (DR2*) and, optionally, the net magnetization
    (DS0) assuming a mono-exponential decay model of the signal, i.e. linear dependence of
    the BOLD signal on the echo time (TE).

  * It is also recommended to read the help of 3dPFM to understand its functionality.
  
  * The ideas behind 3dMEPFM are described in the following papers:
      
   - For a comprehensive description of the algorithm, based on a model that
     only considers fluctuations in R2* (DR2*) and thus only estimates DR2*
     (i.e. this model is selected with option -R2only), see:

        C Caballero-Gaudes, S Moia, P. Panwar, PA Bandettini, J Gonzalez-Castillo
        A deconvolution algorithm for multiecho functional MRI: Multiecho Sparse Paradigm Free Mapping
        (submitted to Neuroimage)

   - For a model that considers both fluctuations in the net magnetization (DS0) and R2*,
     but only imposes a regularization term on DR2* (setting -rho 0 and without -R2only), 
     see

        C Caballero-Gaudes, PA Bandettini, J Gonzalez-Castillo
        A temporal deconvolution algorithm for multiecho functional MRI
        2018 IEEE 15th International Symposium on Biomedical Imaging (ISBI 2018)
        https://ieeexplore.ieee.org/document/8363649
     
   - For a model that considers both fluctuations in the net magnetization (DS0) and R2*,
     and imposes regularization terms on DR2* and DS0 (i.e. setting rho > 0, and without -R2only),
     see

     The results of this paper were obtained with rho = 0.5
        
        C Caballero-Gaudes, S. Moia, PA Bandettini, J Gonzalez-Castillo
        Quantitative deconvolution of fMRI data with Multi-echo Sparse Paradigm Free Mapping
        Medical Image Computing and Computer Assisted Intervention (MICCAI 2018)
        Lecture Notes in Computer Science, vol. 11072. Springer
        https://doi.org/10.1007/978-3-030-00931-1_36
    
  * IMPORTANT. This program is written in R. Please follow the guidelines in

        http://afni.nimh.nih.gov/sscc/gangc/Rinstall.html

    to install R and make AFNI compatible with R. Particularly, the "snow" library 
    must be installed for parallelization across CPU nodes. 

       install.packages("snow",dependencies=TRUE)

    In addition, you need to install the following libraries with dependencies:

       install.packages("abind",dependencies=TRUE)
       install.packages("lars",dependencies=TRUE)
       install.packages("wavethresh",dependencies=TRUE)

    Also, if you want to run the program with the options "rho > 0", you must install
    the R package of the generalized lasso (https://projecteuclid.org/euclid.aos/1304514656)  
    This package was removed from CRAN repository, but the source code is available in:

       https://github.com/glmgen/genlasso
  '
  
  ex1 <-
    "
  Example usage with a dataset with 3 echoes:
  -----------------------------------------------------------------------------
  3dMEPFM  -input data_TE1.nii 0.015      \
           -input data_TE2.nii 0.030      \
           -input data_TE3.nii 0.045      \
           -mask mask.nii                 \
           -criteria bic                  \
           -hrf SPMG1                     \
           -jobs 1                        

  \n"


  parnames <- names(params)
  ss <- vector('character')
  if (alpha) {
    parnames <- sort(parnames)
    ss <- paste('Options in alphabetical order:\n',
                '------------------------------\n', sep='')
  } else {
    ss <- paste('Options:\n',
                '--------\n', sep='')
  }
  for (ii in 1:length(parnames)) {
    op <- params[parnames[ii]][[1]]
    if (!is.null(op$help)) {
      ss <- c(ss , paste(itspace, op$help, sep=''));
    } else {
      ss <- c(ss, paste(itspace, parnames[ii],
                        '(no help available)\n', sep=''));
    }
  }
  ss <- paste(ss, sep='\n');
  cat(intro, ex1, ss, sep='\n');

  if (adieu) exit.AFNI();
}

#Change command line arguments into an options list
read.RprogDemo.opts.batch <- function (args=NULL, verb = 0) {
  params <- list (

    '-input' = apl(n = 2, d = NA, dup = TRUE, h = paste (
      "-input DSET TE                       \n",
      "     DSET: Dataset to analyze with Multiecho Paradigm Free Mapping.      \n",
      "     and the corresponding TE. DSET can be any of the formats available  \n",
      "     in AFNI, e.g: -input Data+orig                                      \n",
      "     TE: echo time of dataset in seconds                                 \n",
      "     Also .1D files where each column is a voxel timecourse.             \n",
      "     If an .1D file is input, you MUST specify the TR with option -TR.   \n"
    ) ),

       '-dbgArgs' = apl(n=0, h = paste( # Gang Chen: added May 19, 2022
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dMEPFM.dbg.AFNI.args in the current directory",
   "          so that debugging can be performed.\n", sep='\n')),

    '-mask' = apl(1, h = paste(
      "-mask MASK: Process voxels inside this mask only. Default is no masking. \n"
    ) ),

    '-penalty' = apl(1, d = NA, h = paste(
      "-penalty PEN:  Regularization term (a.k.a. penalty) for DR2 & DS0        \n",
      "     * Available options for PEN are:                                    \n",
      "         lasso:            LASSO (i.e. L1-norm)                          \n",
      "     * If you are interested in other penalties (e.g. ridge regression,  \n",
      "       fused lasso, elastic net), contact c.caballero@bcbl.eu            \n"
    ) ),
    
    '-criteria' = apl(1, d = 'bic', h = paste(
      "-criteria CRIT:  Model selection of the regularization parameter.        \n",
      "     * Available options are:                                            \n",
      "         bic:  Bayesian Information Criterion (default)                  \n",
      "         aic:  Akaike Information Criterion                              \n",
      "         mad:  Regularization parameter is selected as the iteration     \n",
      "               that makes the standard deviation of the residuals to be  \n",
      "               larger than factor_MAD * sigma_MAD, where sigma_MAD is    \n",
      "                the MAD estimate of the noise standard deviation         \n",
      "                (after wavelet decomposition of the echo signals)        \n",
      "         mad2: Regularization parameter is selected so that              \n",
      "                the standard deviation of the residuals is the closest   \n",
      "                to factor_MAD*sigma_MAD.                                 \n",
      "     * If you want other options, contact c.caballero@bcbl.eu            \n"
    ) ),

    '-maxiterfactor' = apl(1, d = 1, h = paste(
      "-maxiterfactor MaxIterFactor (between 0 and 1):                          \n",
      "     * Maximum number of iterations for the computation of the           \n",
      "       regularization path will be 2*MaxIerFactor*nscans                 \n",
      "     * Default value is MaxIterFactor = 1                                \n"
    ) ),

    '-TR' = apl(n=1, d = 1, h = paste(
      "-TR tr:  Repetition time or sampling period of the input data            \n",
      "     * It is required for the generation of the deconvolution HRF model. \n",
      "     * If input datasets are .1D file, TR must be specified in seconds.  \n",
      "       If TR is not given, the program will STOP.                        \n",
      "     * If input datasets are 3D+time volumes and TR is NOT given,        \n",
      "       the value of TR is taken from the dataset header.                 \n",
      "     * If TR is specified and it is different from the TR in the header  \n",
      "       of the input datasets, the program will STOP.                     \n"
    ) ),

    '-hrf' = apl(n=1,d = 1, h=paste(
      "-hrf fhrf:   haemodynamic response function used for deconvolution       \n",
      "     *  fhrf can be any of the HRF models available in 3dDeconvolve.     \n",
      "     http://afni.nimh.nih.gov/pub/dist/doc/program_help/3dDeconvolve.html\n",
      "     i.e. 3dMEPFM calls 3dDeconvolve with options -x1D_stop & -nodata    \n",
      "     to create the HRF with onset at 0 (i.e. -stim_time 1 '1D:0' fhrf )  \n",
      "     *  [Default] fhrf == 'GAM', the 1 parameter gamma variate  \n",
      "                     (t/(p*q))^p * exp(p-t/q)                            \n",
      "                    with p=8.6 q=0.547 if only 'GAM' is used             \n",
      "                 ** The peak of 'GAM(p,q)' is at time p*q after          \n",
      "                    the stimulus.  The FWHM is about 2.3*sqrt(p)*q.      \n",
      "     *  Another option is fhrf == 'SPMG1', the SPM canonical HRF.        \n",
      "     *  If fhrf is a .1D, the program will use it as the HRF model.      \n",
      "            ** It should be generated with the same TR as the input data \n",
      "               to get sensible results (i.e. know what you are doing).   \n",
      "            ** fhrf must be column or row vector, i.e. only 1 hrf allowed.\n",
      "     * The HRF is normalized to maximum absolute amplitude equal to 1.   \n"
    )),

    # '-hrf_vol' = apl(n=1,d = 1, h=paste(
    #   "-hrf_vol hrf_DSET: 3D+time dataset with voxel dependent HRFs.            \n",
    #   "     * The grid and TR of hrf_DSET must be the same as the input dataset.\n",
    #   "     * This dataset can be the output of -iresp option in 3dDeconvolve,  \n",
    #   "       which contains the estimated HRF (a.k.a. impulse responses)       \n",
    #   "     * In 3dMEPFM, the HRF response is assumed constant during the run.  \n",
    #   "     * See also -idx_hrf, another option to use voxel dependent HRFs.    \n"
    # )),
    #
    # '-idx_hrf' = apl(n=1,d = 1, h=paste(
    #   "-idx_hrf idx_hrf_DSET:   3D dataset with voxel-dependent indexes that    \n",
    #   "      indicates which column of the .1D file in option -hrf should be    \n",
    #   "      used for each voxel.                                               \n",
    #   "     * The grid of idx_hrf_DSET must be the same as the input dataset.   \n",
    #   "     * The number of HRFs in  -hrf must be <= maximum index in idx_hrf_DSET.\n",
    #   "       Otherwise, the program will STOP before starting any calculation. \n",
    #   "     * Only positive integers > 0 are allowed in this option.            \n",
    #   "     * E.g, this dataset can be created by clustering (e.g. 3dKmeans)    \n",
    #   "       the estimated HRF genereted with option -iresp in 3dDeconvolve.   \n",
    #   "     * In 3dMEPFM, the HRF response is assumed constant during the run.  \n",
    #   "     * An index equal to 1 will select the first column of the .1D fhrf, \n",
    #   "       which is usually column 0 in AFNI nomenclature.                   \n"
    # )),

    '-R2only' = apl(0, d = FALSE, h = paste(
      "-R2only:                                                                 \n",
      "     * If this option is given, the model will only consider R2* changes \n",
      "       and do not estimate S0 changes.                                   \n"
    ) ),

    '-rho' = apl(1, d = 0, h = paste(
      "-rho:    0 <= rho <= 1 (default 0):                                      \n",
      "     * Parameter that balances the penalization of the DS0 (rho) and     \n",
      "       DR2star (1-rho) coefficients.                                     \n",
      "     * Default is rho = 0, i.e. no penalization of DS0 coefficients.     \n",
      "     * It becomes irrelevant with -R2only option.                        \n"
    ) ),

    '-factor_min_lambda' = apl(1, d = 0.1, h = paste(
      "-factor_min_lambda  value >= 0      (default factor_min_lambda = 0.1):   \n",
      "     * Stop the computation of the regularization path when              \n",
      "       lambda <= factor_min_lambda*sigma_MAD, where sigma_MAD is the     \n",
      "        estimate of the standard deviation of the noise (computed after  \n",
      "        wavelet decomposition). It must be equal to or larger than 0.    \n"
    ) ),

    '-factor_MAD' = apl(1, d = 1, h = paste(
      "-factor_MAD        (default factor_MAD = 1):                             \n",
      "     *  For criteria 'mad', select lambda so that the standard deviation \n",
      "        of residuals is approximately equal to factor_MAD*sigma_MAD      \n"
    ) ),

    '-debias_TEopt' = apl(1, d = 0, h = paste(
      "-debias_TEopt: 0 <= debias_TEopt <= number of input datasets             \n",
      "     * For debiasing, only consider the 'debias_TEopt' input dataset,    \n",
      "       i.e. if debias_TEopt=2, the dataset corresponding to the second   \n",
      "       TE will be used for debiasing. This option is available in case   \n",
      "       you really know that one of the echoes is the 'optimal' TE  ...   \n",
      "       As if this information was easy to know and match :)              \n",
      "     * Default is debias_TEopt = 0, i.e. all echoes will be considered.  \n",
      "     * This option is not recommended unless you understand it,          \n",
      "       (i.e. use at your own risk)                                       \n"
    ) ),

    '-do_prior_debias' = apl(0, d = FALSE, h = paste(
      "-do_prior_debias:                                                        \n",
      "     * If this option is given, the algorithm will perform debiasing     \n",
      "       before the selection of the regularization parameter.             \n",
      "     * This option is not recommended unless you understand it,          \n",
      "       (i.e. use at your own risk)                                       \n"
    ) ),

    '-n_selection_Nscans' = apl(0, d = FALSE, h = paste(
      "-n_selection_Nscans:                                                     \n",
      "     * The equation for model selection for selection of regularization  \n",
      "       parameter with the 'bic' and 'aic' criteria depends on the number \n",
      "       of observations (i.e. number of scans * number of echoes)         \n",
      "     * If -n_selection_Nscans is given, the formula will assume that     \n",
      "       the number of observations is the number of scans. This is        \n",
      "       mathematically wrong, but who cares if it gives better results!!  \n",
      "     * This option is not recommended unless you understand it,          \n",
      "       (i.e. use at your own risk)                                       \n"
    ) ),

    #      '-stim_file' = apl(n = 1, d = 1, h = paste(
    #   "-stim_file sf.1D: 1D file with the ideal time series of one stimulus condition.\n",
    #   "   * This is particularly useful for PPI analysis where the time series      \n",
    #   "     modelling the psycho-physiological interaction is the multiplication    \n",
    #   "     of deconvolved time series s(t) and ideal stimulus time series sf(t),   \n",
    #   "                   ppi(t) = s(t) * sf(t)                                     \n",
    #   "   * In this case, the most common way of using 3dMEPFM will be to give     \n",
    #   "     a single 1D file as -input, representing a voxel- or region-of-interest.\n",
    #                    ) ),
    
    '-prefix' = apl (n = 1, d = NA, dup = TRUE, h = paste (
      "-prefix                                                                  \n",
      "     * The names of the output volumes will be generated automatically   \n",
      "       as outputname_prefix_input, e.g. if -input = TheEmperor+orig,     \n",
      "       and prefix is Zhark, the names of the DR2 output volumes is       \n",
      "             DR2_Zhark_TheEmperor+orig for  volume                       \n",
      "       whereas if prefix is not given, the output will be                \n",
      "             DR2_TheEmperor+orig.                                        \n",
      "     * The program will automatically save the following volumes:        \n",
      "       -DR2      Changes in R2* parameter, which is assumed to           \n",
      "                 represent neuronal-related signal changes.              \n",
      "       -DR2fit   Convolution of DR2 with HRF, i.e. neuronal-related      \n",
      "                 haemodynamic signal. One volume per echo is created.    \n",
      "       -DS0      Changes in net magnetization (S0) (if estimated)        \n",
      "       -lambda   Regularization parameter                                \n",
      "       -sigmas_MAD  Estimate of the noise standard deviation after       \n",
      "                   wavelet decomposition for each input dataset          \n",
      "       -costs      Cost function to select the regularization parameter  \n",
      "                   (lambda) according to selection criterion             \n",
      "     * If you don't want to delete or rename anyone, you can always      \n",
      "       delete them later or rename them with 3dcopy.                     \n"
    ) ),
    
    '-jobs' = apl(n = 1, d = 1, h = paste(
      "-jobs NJOBS: On a multi-processor machine, parallel computing will       \n",
      "             speed up the program significantly.                         \n",
      "             Choose 1 for a single-processor computer (DEFAULT).         \n"
    ) ),
    
    '-nSeg' = apl(n = 1, d = 1, h = paste(
      "-nSeg XX: Divide into nSeg segments of voxels to report progress,        \n",
      "          e.g. nSeg 5 will report every 20% of proccesed voxels.         \n",
      "          Default = 10                                                   \n"
    ) ),

    # '-block' = apl(0, d = FALSE, h = paste(
    #   "-block:                                                                  \n",
    #   "     * Use integration operator in design matrix.                        \n",
    #   "     * May be useful for sustained events.                               \n",
    #   "     * It estimates innovation signals, i.e, the derivative of DR2.      \n",
    # ) ),

    '-verb' = apl(n=1, d = 0, h = paste(
      "-verb VERB: VERB is an integer specifying verbosity level.\n",
      "            0 for quiet, 1 (default) or more: talkative.\n"
    ) ),

    '-help' = apl(n=0, h = '-help: this help message\n'),

    '-show_allowed_options' = apl(n=0, h=
              "-show_allowed_options: list of allowed options\n" )

  );

  ops <- parse.AFNI.args(args,  params,
                         other_ok=FALSE
  )

  if (verb) show.AFNI.args(ops, verb=0, hstr='');
  if (is.null(ops)) {
    errex.AFNI('Error parsing arguments. See 3dMEPFM -help for details.\n');
  }

  #initialize with defaults
  com_history <-AFNI.command.history(ExecName, args,NULL)
  lop <- AFNI.new.options.list(history = com_history, parsed_args = ops)
  lop$input <- NULL
  lop$dbgArgs <- FALSE # Gang Chen: for debugging purpose
  lop$TR <- NULL
  lop$mask <- NULL
  lop$nNodes <- 1
  lop$nSeg <- 10
  lop$hrf <- NULL
  lop$vol_hrfs <- NULL
  lop$idx_hrf <- NULL
  lop$penalty <- 'lasso'
  lop$criteria <- 'bic'
  lop$maxiterfactor <- 1
  lop$debias_TEopt <- NULL
  lop$do_prior_debias <- FALSE
  lop$R2only <- FALSE
  lop$block <- FALSE
  lop$rho <- 0
  lop$factor_MAD <- 1
  lop$factor_min_lambda <- 0.1
  lop$n_selection_Nscans <- FALSE
  lop$verb <- 1
  lop$iometh <- 'clib'

  #initialize default names for output datasets

  lop$prefix <- NULL

  #Get user's input
  for (i in 1:length(ops)) {
    opname <- strsplit(names(ops)[i],'^-')[[1]];
    opname <- opname[length(opname)];
    switch(opname,

           input  = lop$input <- c(lop$input,ops[[i]]),
	   dbgArgs = lop$dbgArgs <- TRUE,
           TR = lop$TR <- ops[[i]],
           mask = lop$mask <- ops[[i]],
           maxiterfactor = lop$maxiterfactor <- ops[[i]],
           penalty = lop$penalty <- ops[[i]],
           criteria = lop$criteria <- ops[[i]],
           debias_TEopt = lop$debias_TEopt <- ops[[i]],
           rho = lop$rho <- ops[[i]],
           factor_MAD = lop$factor_MAD <- ops[[i]],
           factor_min_lambda = lop$factor_min_lambda <- ops[[i]],
           n_selection_Nscans = lop$n_selection_Nscans <- TRUE,
           do_prior_debias = lop$do_prior_debias <- TRUE,
           R2only = lop$R2only <- TRUE,
           block = lop$block <- TRUE,
           hrf = lop$hrf <- ops[[i]],
           idx_hrf = lop$idx_hrf <- ops[[i]],
           hrf_vol = lop$vol_hrfs <- ops[[i]],
           verb = lop$verb <- ops[[i]],
           jobs = lop$nNodes <- ops[[i]],
           nSeg = lop$nSeg <- ops[[i]],
           prefix = lop$prefix <- pprefix.AFNI.name(ops[[i]]),
           help = help.RprogDemo.opts(params, alpha=FALSE, adieu=TRUE),
           show_allowed_options = show.AFNI.args(ops, verb=0,
                                                 hstr="3dMEPFM's",adieu=TRUE)
    )
  }

  if (length(lop$input) < 1) {
    if (lop$verb) {
      str(lop)
      errex.AFNI('No input? Check dumped input struct above. \n');
    } else {
      errex.AFNI('No input? \n');
    }
    return(NULL)
  }


  return(lop)
}# end of read.RprogDemo.opts.batch

#################################################################################
############### Begin MEPFM Computation functions ###########################
#################################################################################

# The big function
Rprog.MEPFM <- function( inData, infoDeconv = NULL) {

  # Put infoDeconv parameters into variables
  TEs <- infoDeconv$TEs
  n_echoes <- length(TEs)
  nscans <- infoDeconv$nscans

  penalty <- infoDeconv$penalty
  criteria <- infoDeconv$criteria
  maxiter <- infoDeconv$maxiter
  times_for_debias <- infoDeconv$times_for_debias
  idx2normalize <- infoDeconv$idx2normalize
  do_prior_debias <- infoDeconv$do_prior_debias
  R2only <- infoDeconv$R2only
  block <- infoDeconv$block
  n_selection_Nscans <- infoDeconv$n_selection_Nscans
  factor_MAD <- infoDeconv$factor_MAD
  factor_min_lambda <- infoDeconv$factor_min_lambda
  hrf_mtx_ME <- infoDeconv$hrf_mtx_ME
  hrf_mtx_ME_norm <- infoDeconv$hrf_mtx_ME_norm
  D <- infoDeconv$D
  rho <- infoDeconv$rho

  inData <- as.matrix(inData)
  # split inData of input time series and voxel-dependent additional regressors
  yi <- inData[,1,drop=FALSE] # Echo signals are already concatenated

  # if (any(as.logical(infoLHS$isVoxelDependent))) {
  #   if (infoDeconv$HRFvoxeldependent){
  #     LHSdependent <- inData[,2:(NCOL(inData)-1),drop=FALSE]
  #   } else {
  #     LHSdependent <- inData[,2:NCOL(inData),drop=FALSE]
  #   }
  # }

  # if (!is.null(infoLHS)) {
  #   ncolsLHS <- sum(infoLHS$ncols) # total number of additional regressors
  # } else {
  #   ncolsLHS <- 0
  # }

  # extract or compute hrf_mtx depending HRf is voxel_dependent or not
  # if (infoDeconv$HRFvoxeldependent){
  #   if (infoDeconv$HRFvoxelidx){
  #     # hrf is indicated with index. Need to choose matrix in hrf_mtx
  #     hrf_mtx <- hrf_mtx[,,inData[1,NCOL(inData)]]
  #     L_hrf <- infoDeconv$L_hrf
  #   } else {
  #
  #     # HRF is different for each voxel, so we need to build the matrix
  #     L_hrf <- infoDeconv$L_hrf
  #     hrf_TR <- inData[1:L_hrf,NCOL(inData),drop=FALSE]
  #     abs_hrf_TR <- abs(hrf_TR)
  #     idx_zeros_hrf <- which(abs_hrf_TR < 10*.Machine$double.eps)
  #     # change zero values in HRF to minimal precision values
  #     if (length(idx_zeros_hrf)>0){
  #       cat(sprintf("Warning: Zero values in HRF found. Changed for numerical stability . \n"))
  #       min_hrf <- max(1e-9*min(abs_hrf_TR[idx_zeros_hrf]), 10*.Machine$double.eps)
  #       hrf_TR[idx_zeros_hrf] <- min_hrf * sign(hrf_TR[idx_zeros_hrf])
  #     }
  #     # normalize to unit peak
  #     hrf_TR <- hrf_TR / max(abs_hrf_TR)
  #     # create Toeplitz matrix
  #     hrf_mtx <- matrix(0,nscans+2*(L_hrf-1),nscans+L_hrf-1)
  #     for(i in 1:(nscans+L_hrf-1)) {
  #       hrf_mtx[i:(i+L_hrf-1),i] <- hrf_TR
  #     }
  #     hrf_mtx <- hrf_mtx[L_hrf:(nscans+L_hrf-1),1:(nscans+L_hrf-1)]
  #   }
  # } else {
  #   L_hrf <- infoDeconv$L_hrf
  # }

  # Define output list to return results and initialize all values to zero
  outpfm <- list()
  outpfm$DR2 <- rep(0,nscans)
  outpfm$DS0 <- rep(0,nscans)
  for (ii in 1:n_echoes){
    temp <- paste('DR2fit',ii,sep="")
    outpfm[[temp]] <- rep(0,nscans)
  }
  outpfm$sigmas_MAD <- rep(0,n_echoes)
  outpfm$lambda <- 0
  outpfm$costs <- rep(0,maxiter)

  # if data is constant time series (e.g. zeros due to masking), it is not fitted
  if (!all(abs(yi - mean(yi)) < 10*.Machine$double.eps)) { # if data is not constant

    # save timeseries to temporary file
    #filename <- sprintf('%s.txt', paste(sample(c(0:9,letters,LETTERS),10,replace=TRUE),collapse =""))
    #write.table(yi,file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)

    # # check if the input data included voxel-dependent additional regressors
    # if (!is.null(infoLHS)) {
    #   # ncolsLHS <- sum(infoLHS$ncols) # total number of additional regressors
    #   LHS <- array(0,dim=c(nscans,ncolsLHS))
    #   j <- 1 # index for columns of LHS
    #   iLHSdependent <- 1 # index for extracting additional regressors in LHSdependent
    #   iLHSconstant <- 1 # index for extracting additional regressors in LHSconstant
    #   for (iLHS in 1:length(infoLHS$isVoxelDependent)){
    #     if (infoLHS$isVoxelDependent[iLHS]){
    #       # VOXEL-DEPENDENT (only one column)
    #       LHS[,j:(j+infoLHS$ncols[iLHS]-1)] <- LHSdependent[,iLHSdependent,drop=FALSE]
    #       iLHSdependent <- iLHSdependent + 1
    #     } else {
    #       # NON VOXEL-DEPENDENT
    #       LHS[,j:(j+infoLHS$ncols[iLHS]-1)] <-
    #         LHSconstant[,iLHSconstant:(iLHSconstant+infoLHS$ncols[iLHS]-1),drop=FALSE]
    #       iLHSconstant <- iLHSconstant + infoLHS$ncols[iLHS]
    #     }
    #     j <- j + infoLHS$ncols[iLHS]
    #   }
    # }
    # # remove mean of LHS regressors
    # if (!is.null(infoLHS)) {
    #   LHS <- LHS - rep(colMeans(LHS),rep(nrow(LHS),ncol(LHS)))
    # }

    # # Build general design matrix
    # if (!nomean) { # estimate mean
    #   if (!is.null(infoLHS)) { # with LHS
    #     gen_mtx <- cbind(hrf_mtx, LHS, matrix(1,nscans,1))
    #   } else { # no LHS
    #     gen_mtx <- cbind(hrf_mtx,  matrix(1,nscans,1))
    #   }
    # } else { # Don't estimate mean
    #   if (!is.null(infoLHS)) { # with LHS
    #     gen_mtx <- cbind(hrf_mtx, LHS)
    #   } else { # no LHS
    #     gen_mtx <- hrf_mtx
    #   }
    # }

    # compute MAD estimate of the AWGN noise using wavelet decomposition
    nscan_sigpad <- 2^ceiling(log2(nscans))
    sigmas_MAD <- rep(0,n_echoes)
    # split input signal into the different echoes
    for (ii in 1:n_echoes){
      sigpad_yi <- matrix(0,nscan_sigpad,1)
      sigpad_yi[1:nscans,1] <- yi[((ii-1)*nscans+1):(ii*nscans),1,drop=FALSE]
      wd_yi <- wd(sigpad_yi,type="wavelet",family = "DaubExPhase", filter.number=3)
      FineCoefs <- accessD(wd_yi,lev=wd_yi$nlevels-1)
      sigmas_MAD[ii] <- mad(FineCoefs[-which(FineCoefs==0)])
    }
    # compute average
    sigma_MAD <- mean(sigmas_MAD)

    # compute minimum lambda to compute regularization path
    min_lambda <- factor_min_lambda*sigma_MAD

    #########################################
    # solve optimization problem
    #########################################
    if  ( ( (rho > 0) && (rho < 1) && (penalty == 'lasso') )  || (R2only == TRUE) ) {

      # transformation to LASSO problem is feasible and computation will be faster
      Dinv <- diag(1/diag(D))
      hrf_mtx_ME_norm_Dinv <- hrf_mtx_ME_norm %*% Dinv
      # library('lars')
      # cat(sprintf('Computation regularization path with LARS \n'))
      Lars_out <- lars(hrf_mtx_ME_norm_Dinv,yi,normalize=FALSE,intercept=FALSE,max.steps=maxiter)
      # cat(sprintf('LARS DONE \n'))
      beta_path <- unname(as.matrix(t(Lars_out$beta)))
      attr(beta_path,"scaled:scale") <- NULL # remove attributes so that it is just a matrix as in MATLAB
      # transform-back to obtain generalized lasso solution
      beta_path <- Dinv %*% beta_path
      fit_path <- hrf_mtx_ME_norm %*% beta_path
      dfs <- unname(Lars_out$df)
      lambdas <- unname(Lars_out$lambda)
      lambdas <- c(lambdas,0) # note that lars outputs a number of lambdas that is one element less than
      # the number of estimates since the first estimate is all zeros
      n_estimates <- ncol(beta_path)
      L2res = colSums(abs( (yi %*% matrix(1,1,n_estimates)) - fit_path)^2)

    } else { # run generalized lasso

      # library('genlasso')
      # cat(sprintf('Computing regularization path with Generalized Lasso (Be Patient)\n'))
      GenLasso_out <- genlasso(yi,hrf_mtx_ME_norm,D,svd=TRUE,approx=TRUE,maxsteps=maxiter,minlam=min_lambda)
      # cat(sprintf('Generalized Lasso DONE \n'))
      beta_path <- GenLasso_out$beta
      fit_path <- unname(GenLasso_out$fit)
      dfs <- unname(GenLasso_out$df)
      lambdas <- unname(GenLasso_out$lambda)

      # Generalized LASSO does not shrinks the estimates exactly to 0,
      # but to a very small value that is variable and can be higher
      # than the minimum precision. However, as shown in the Generalized LASSO
      # paper, one can find out which coefficients of each iteration must be
      # exactly 0 based on the following rules:
      # - For LASSO-ish D matrices (e.g. LASSO, Weighted-Fusion, Elastic Net, Smooth LASSO):
      #   beta[,iter] = 0 iff GenLasso_out$u[,iter] != GenLasso_out$lambda[,iter]
      # - For Fused LASSO-ish matrices (e.g. Fused LASSO, Sparse Fused LASSO)
      #   the conditions are different (check GenLASSO_sim.R as I already implemented it)
      #
      # This first version of the program only allows penalty = 'lasso'
      if (penalty == 'lasso') {
        for (iter_est in 1:ncol(beta_path)) {
          nodes2remove <- which(abs(GenLasso_out$u[,iter_est]) != GenLasso_out$lambda[iter_est])
          # Must consider if rows of D have been removed because (rho = 0) or (rho = 1)
          if (rho == 0) {nodes2remove <- nodes2remove + nscans}
          if (length(nodes2remove)>0) beta_path[nodes2remove,iter_est] <- 0
        }
      }

      # Compare with LARS, the generalized lasso algorithm does not output the null estimate
      # Therefore, we must add it manually. This is particularly important if the problem
      # could be transformed to LASSO (see above) and the optimal solution could be the null solution
      # Also, the program could give different solutions depending on the algorithm used

      # this needs to be done depending on rho
      # If rho = 0, then perform LS with the columns corresponding to DS0
      # If rho = 1, then perform LS with the columns corresponding to HRF (although this solution does not make sense)
      # Otherwise, the problem could have been transformed to lasso and we must add the null solution

      if ((rho == 0) && (penalty == 'lasso')){
        temp <- lm(yi ~ hrf_mtx_ME_norm[,1:nscans] + 0)
        beta_path <- cbind(c(unname(temp$coefficients),rep(0,nscans)),beta_path)
        fit_path <- cbind(unname(temp$fitted.values),fit_path)
      }
      if ((rho == 1) && (penalty == 'lasso')){
        temp <- lm(yi ~ hrf_mtx_ME_norm[,(nscans+1):(2*nscans)] + 0)
        beta_path <- cbind(c(rep(0,nscans),unname(temp$coefficients)),beta_path)
        fit_path <- cbind(unname(temp$fitted.values),fit_path)
      }
      if ((rho > 0) && (rho < 1) && (penalty == 'lasso')){
        beta_path <- cbind(rep(0,2*nscans),beta_path)
        fit_path <- cbind(rep(0,n_echoes*nscans),fit_path)
      }
      dfs <- c((dfs[1]-1),dfs)
      lambdas <- c(lambdas[1],lambdas)
      n_estimates <- ncol(beta_path)
      L2res = colSums(abs( (yi %*% matrix(1,1,n_estimates)) - fit_path)^2)
    }

    # WHETHER WE SHOULD EMPLOY NSCANS OR NSCANS*N_ECHOES IN MODEL
    # SELECTION CRITERION WHEN ALL ECHOES ARE CONSIDERED
    N_selection = length(times_for_debias) # this value will either be nscans*n_echoes or nscans
    # however, for 'bic' and 'aic', if n_selection_Nscans == TRUE, N_selection will be nscans
    if (((criteria == 'bic') || (criteria == 'aic')) && (n_selection_Nscans == TRUE)){
      N_selection = nscans
    }

    # if do_prior_debias == TRUE, debiasing is performed prior to model selection
    if (do_prior_debias == TRUE){
      # Initialize solutions for debiasing
      L2res_debias = rep(0,n_estimates)
      for (iter_est in 1:n_estimates) {
        idx_debias = which(abs(beta_path[,iter_est]) > 10*.Machine$double.eps)
        if (length(idx_debias)>0) {
          hrf_mtx_ME_idx_debias = hrf_mtx_ME_norm[times_for_debias,idx_debias,drop = FALSE]
          # linear fit assuming no intercept
          LSfitdebias <- lm(yi[times_for_debias] ~ hrf_mtx_ME_idx_debias + 0)
          # Sum of squares of the residuals
          L2res_debias[iter_est] <- colSums(abs(as.matrix(LSfitdebias$residuals))^2)
        } else {
          L2res_debias[iter_est] <- colSums(abs(yi)^2)
        }
      }
      L2res = L2res_debias
    }

    # selection of regularization parameter
    if (criteria == 'bic') { # Bayesian information criterion
      # compute Residual Sum of Squares
      costs = N_selection*log(L2res) + log(N_selection)*dfs
      idx_opt = which.min(costs)
    }
    if (criteria == 'aic') { # Akaike information criterion
      costs = N_selection*log(L2res) + 2*dfs
      idx_opt = which.min(costs)
    }
    if (criteria == 'mad'){ # find lambda so that variance of residuals AFTER DEBIASING is less than factor_MAD*sigma_MAD
      costs = sqrt(L2res/N_selection) - factor_MAD*sigma_MAD
      costs_NaN <- costs
      costs_NaN[which(costs < 0)] <- NaN
      if (all(is.na(costs_NaN))) {
        idx_opt <- 1
      } else {
        idx_opt = which.min(costs_NaN)
      }
    }
    if (criteria == 'mad2'){ # find lambda so that variance of residuals AFTER DEBIASING is the closest to factor_MAD*sigma_MAD
      costs = abs(sqrt(L2res/N_selection) - factor_MAD*sigma_MAD)
      idx_opt = which.min(costs)
    }

    # optimal lambda
    lambda <- lambdas[idx_opt]
    # optimal estimates
    x_opt <- beta_path[,idx_opt,drop=FALSE]

    # # Apply inverse transformation as columns of design matrix were normalized to unit norm
    # x_opt[idx2normalize] <- diag(1/norm_hrf_mtx_ME[idx2normalize]) %*% x_opt[idx2normalize]
    #
    # D_rho <- x_opt[1:nscans,,drop=FALSE]
    # D_kappa <- x_opt[(nscans+1):(2*nscans),,drop=FALSE]
    # DS0 <- D_rho
    # DR2star <- D_kappa/mean(TEs)

    # final debiasing
    if (R2only == FALSE) {
      x_opt_debias <- matrix(0,2*nscans,1)
    } else {
      x_opt_debias <- matrix(0,nscans,1)
    }
    idx_debias <- which(abs(x_opt) > 10*.Machine$double.eps)
    if (length(idx_debias) > 0) {
      hrf_mtx_ME_idx_debias = hrf_mtx_ME[times_for_debias,idx_debias,drop=FALSE]
      LSfitdebias <- lm(yi[times_for_debias] ~ hrf_mtx_ME_idx_debias + 0)
      x_opt_debias[idx_debias,] <- unname(LSfitdebias$coefficients) # extract estimates
    }
    if (R2only == FALSE) {
      # DS0 and DR2
      D_rho_debias <- x_opt_debias[1:nscans,drop=FALSE]
      DS0_debias <- D_rho_debias
      D_kappa_debias <- x_opt_debias[(nscans+1):(2*nscans),drop=FALSE]
      DR2star_debias <- D_kappa_debias/mean(TEs)
      # Fitted signals
      # DR2starHRF <- hrf_mtx_ME[,(nscans+1):(2*nscans)] %*% D_kappa
      DR2starHRF_debias <- hrf_mtx_ME[,(nscans+1):(2*nscans)] %*% D_kappa_debias
      DR2starHRF_debias <- matrix(DR2starHRF_debias,nscans,n_echoes)
    } else {
      # DR2
      D_kappa_debias <- x_opt_debias[1:nscans,drop=FALSE]
      DR2star_debias <- D_kappa_debias/mean(TEs)
      DS0_debias <- rep(0,nscans)
      # Fitted signals
      # DR2starHRF <- hrf_mtx_ME[,(nscans+1):(2*nscans)] %*% D_kappa
      DR2starHRF_debias <- hrf_mtx_ME %*% D_kappa_debias
      DR2starHRF_debias <- matrix(DR2starHRF_debias,nscans,n_echoes)
    }

    # return optimal values
    outpfm$DR2 <- DR2star_debias
    outpfm$DS0 <- DS0_debias
    for (ii in 1:n_echoes){
      temp <- paste('DR2fit',ii,sep="")
      outpfm[[temp]] <- DR2starHRF_debias[,ii]
    }
    if (length(costs) > maxiter) { # prevent error due to length of costs is larger than maxiter
      costs <- costs[1:maxiter]
    }
    outpfm$costs[1:length(costs)] <- costs
    outpfm$lambda <- lambda
    outpfm$sigmas_MAD <- sigmas_MAD

  } # End IF data is zero or constant

  return(outpfm);
}

#################################################################################
######################## Begin RprogDemo main ##################################
#################################################################################
if (!exists('.DBG_args')) {
  args = (commandArgs(TRUE))
  rfile <- first.in.path(sprintf('%s.R',ExecName))
  if ( '-dbgArgs' %in% args ) {
     try(save(args, rfile, file=sprintf('.%s.dbg.AFNI.args',ExecName), ascii = TRUE), silent=TRUE)
  }
} else {
  note.AFNI("Using .DBG_args resident in workspace");
  args <- .DBG_args
}
if (!length(args)) {
  errex.AFNI('Error parsing interactive input.');
} else {
  if (!exists('.DBG_args')) {
    BATCH_MODE <<- 1
  } else {
    BATCH_MODE <<- 0
  }
  if (is.null(lop <- read.RprogDemo.opts.batch(args, verb = 0))) {
    errex.AFNI('Error parsing input');
  }
  #str(lop);
}

if (lop$verb > 1) {
  str(lop);
}

# parse lop$input into datasets names & TEs
if ((length(lop$input) >= 2) && ((length(lop$input)%%2)==0)) {
  n_inputs <- length(lop$input)%/%2
  lop$datasets <- NULL
  lop$TEs <- NULL
  for (iter_input in 1:n_inputs){
    lop$datasets <- c(lop$datasets,lop$input[2*(iter_input-1)+1])
    lop$TEs <- c(lop$TEs,lop$input[2*iter_input])
  }
  lop$TEs <- sort(as.numeric(lop$TEs),index.return=TRUE)
  lop$idx_order_TEs <- lop$TEs$ix
  lop$TEs <- lop$TEs$x
} else {
  errex.AFNI('Format of -input is not appropriate.\n')
}

###########################################################################################
# load third-party libraries
cat('Loading abind library. Required for combining multi-dimensional arrays.\n')
library('abind')
# load appropriate libraries
cat(sprintf('Loading lars package. Required for LASSO.\n'))
library('lars')
if (lop$rho) {
  cat(sprintf('Loading genlasso package. Required for Generalized LASSO.\n'))
  library('genlasso')
}
cat(sprintf('Loading wavethresh package. Required for wavelet analysis.\n'))
library(wavethresh)

###########################################################################################
# check number of nodes in case jobs < 1
if(lop$nNodes < 1) {
  lop$nNodes <- 1
  cat('-jobs < 1?? No CPU??. Setting number of jobs to 1.\n')
}

###########################################################################################
# Load input dataset
for (iter_input in 1:n_inputs){
  cat(sprintf('Loading input dataset %s \n',lop$datasets[iter_input]))
  idset <- read.AFNI(lop$datasets[lop$idx_order_TEs[iter_input]], verb = max(0,lop$verb-1), meth=lop$iometh)
  ###########################################################################################
  #For convenience, change the dimensions so that the first 3
  #dimensions are turned into 1
  ddi <- dset.dimBRKarray(idset)
  idset <- dset.3DBRKarrayto1D(idset)
  dd <- dim(idset$brk)
  nscans <- dd[2]  # number of volumes in input dataset
  nvoxels <- dd[1]  # number of voxels in input dataset
  if (iter_input == 1){
    nscans_master <- nscans
    nvoxels_master <- nvoxels
    idset_master <- idset
    idsets <- idset
  } else {
    # check that nscans and nvoxels is the same as the first dataset
    if ((nscans == nscans_master) && (nvoxels == nvoxels_master)){
      if (!dset.gridmatch(idsets, idset)) {
        errex.AFNI(sprintf("Mismatch between grids of mask %s and input %s \n",lop$dataset[1],lop$datasets[iter_input]));
      } else {
        # concatenate dataset in the time dimension of idset
        idsets$brk <- cbind(idsets$brk,idset$brk)
      }
    } else{
      errex.AFNI('Input datasets do not have same number of voxels or number of scans.\n')
    }
  }
}

# delete idset to save memory
rm(idset)

# set default values of criteria and penalty if NULL
if (is.null(lop$criteria)){
  lop$criteria = 'bic'
}
if (is.null(lop$penalty)){
  lop$penalty = 'lasso'
}

###########################################################################################
#Set TR value, given as parameter for input 1D files or use TR from header of input dataset
if ((substr(lop$input[1],nchar(lop$input[1])-2,nchar(lop$input[1]))==".1D") ||
      (substr(lop$input[1],nchar(lop$input[1])-4,nchar(lop$input[1]))==".1D\'")){
  if (is.null(lop$TR)){
    if (lop$verb) {
      str(lop)
      errex.AFNI('Input is .1D file. No TR given? You must specify TR in seconds!!');
    } else {
      errex.AFNI('Input is .1D file. No TR given?');
    }
    return(NULL)
  } else {
    TR <- lop$TR
  }
} else {
  TR <- dset.attr(idsets,'TR')
  if (!is.null(lop$TR)){
    if (TR != lop$TR) {
      errex.AFNI(sprintf("Input -TR is different from TR in header of %s \n",lop$datasets[1]))
    }
  }
}

###########################################################################################
#Load mask, check dimensions and apply mask
mdset <- NULL
if (!is.null(lop$mask)) {
  if (lop$verb) {
    cat(sprintf("Loading and applying mask %s \n",lop$mask));
  }
  mdset <- read.AFNI(lop$mask, verb = max(lop$verb-1), meth=lop$iometh)
  mdset <-  dset.3DBRKarrayto1D(mdset);
  if (!dset.gridmatch(mdset, idsets)) {
    errex.AFNI(sprintf("Mismatch between grids of mask %s and input %s \n",lop$mask,lop$datasets[1]));
  } else {
    idsets$brk <- (mdset$brk%*%matrix(1,1,nscans*n_inputs))*idsets$brk
  }
}

###########################################################################################
# Create list with information about the HRF deconvolution algorithm
infoDeconv <- list()

###########################################################################################
# HRF options
if (  ( (!is.null(lop$hrf)) && (!is.null(lop$vol_hrfs)) ) ||
        ( (!is.null(lop$idx_hrf)) && (!is.null(lop$vol_hrfs)) ) ) {
  errex.AFNI(sprintf("Option -hrf_vol is INCOMPATIBLE with -hrf or -idx_hrf.\n"))
}

# If none of the -hrf, -idx_hrf or -hrf_vol options is given, set 'GAM' as default HRF
if ( (is.null(lop$hrf)) && (is.null(lop$idx_hrf)) && (is.null(lop$vol_hrfs)) ) {lop$hrf <- 'GAM'}

# only -hrf option, no voxel-dependent HRF, the same across the entire brain
if ( (!is.null(lop$hrf)) && (is.null(lop$idx_hrf)) ) {

  if ((substr(lop$hrf,nchar(lop$hrf)-2,nchar(lop$hrf))==".1D") ||
        (substr(lop$hrf,nchar(lop$hrf)-4,nchar(lop$hrf))==".1D\'")) {
    hrf_TR <- read.AFNI.matrix(lop$hrf)
    if ((NCOL(hrf_TR) > 1) && (NROW(hrf_TR) > 1)) {
      errex.AFNI(sprintf("More than one hrf in %s without -idx_hrf option. So, it must be column or row vector.\n",lop$hrf))
    } else {
      if (NCOL(hrf_TR) > 1) {
        hrf_TR <- t(hrf_TR)
      }
    }
  } else {
    # check if 16 sec was enough to describe the specified HRF, i.e. last HRF sample is equal to zero.
    # Otherwise, increase duration until the last HRF sample is zero
    dur_HRF <- 8
    last_HRF_sample <- 1
    while (last_HRF_sample != 0) {
      dur_HRF <- 2*dur_HRF
      npoints_HRF <- round(dur_HRF / TR)
      HRF_str = sprintf("3dDeconvolve -x1D_stop -nodata %d %f -polort -1  -num_stimts 1 -stim_times 1 '1D:0' '%s' -quiet -x1D stdout: | 1deval -a stdin: -expr 'a'",dur_HRF,TR,lop$hrf)
      hrf_TR <- as.matrix(as.numeric(system(HRF_str,intern=TRUE)),ncol=1)
      last_HRF_sample <- hrf_TR[length(hrf_TR)]
      if (last_HRF_sample != 0) {
        cat(sprintf('Duration of HRF was not sufficient for specified model. Doubling duration and computing again.\n'))
      }
    }
    # remove zero samples at the end of the HRF
    while (last_HRF_sample == 0) {
      hrf_TR <- hrf_TR[1:(length(hrf_TR)-1),1,drop=FALSE]
      last_HRF_sample <- hrf_TR[length(hrf_TR)]
    }
  }
  # normalize to unit peak
  hrf_TR <- hrf_TR / max(hrf_TR)
  L_hrf <- length(hrf_TR)
  # create Toeplitz convolution matrix
  hrf_mtx <- matrix(0,nscans+L_hrf-1,nscans)
  for(i in 1:(nscans)) {
    hrf_mtx[i:(i+L_hrf-1),i] <- hrf_TR
  }
  hrf_mtx <- hrf_mtx[1:nscans,1:nscans]

  # save in infoDeconv
  infoDeconv$HRFvoxeldependent <- 0 # HRF is not voxel dependent
  infoDeconv$HRFvoxelidx <- 0 # No volume of indexes to select among multiple HRF
  infoDeconv$hrf_mtx <- hrf_mtx
  infoDeconv$L_hrf <- L_hrf

  if (lop$R2only == FALSE) {

    # create model matrix for multi-echo formulation with DR2 and DS0
    hrf_mtx_ME <- matrix(0,n_inputs*nscans,2*nscans)
    for (iter_input in 1:n_inputs){
      hrf_mtx_ME[(((iter_input-1)*nscans)+1):(iter_input*nscans),(1:nscans)] = diag(1,nscans)
      if (lop$block == TRUE) {
        hrf_mtx_ME[(((iter_input-1)*nscans)+1):(iter_input*nscans),(nscans+1):(2*nscans)] = -(lop$TEs[iter_input]/mean(lop$TEs))*hrf_mtx %*% lower.tri(hrf_mtx, diag=TRUE)
      } else {
        hrf_mtx_ME[(((iter_input-1)*nscans)+1):(iter_input*nscans),(nscans+1):(2*nscans)] = -(lop$TEs[iter_input]/mean(lop$TEs))*hrf_mtx
      }
    }
    infoDeconv$hrf_mtx_ME <- hrf_mtx_ME
    # normalize columns to unit norm
    hrf_mtx_ME_norm = matrix(0,n_inputs*nscans,2*nscans)
    norm_hrf_mtx_ME <- sqrt(diag(t(hrf_mtx_ME) %*% hrf_mtx_ME))
    # also keep track coefficients where unit-norm normalization was applied (i.e. norm > 0)
    idx2normalize <- which(norm_hrf_mtx_ME > 0)
    hrf_mtx_ME_norm[,idx2normalize] <- hrf_mtx_ME[,idx2normalize] %*% diag(1/norm_hrf_mtx_ME[idx2normalize])
    infoDeconv$idx2normalize <- idx2normalize
    infoDeconv$hrf_mtx_ME_norm <- hrf_mtx_ME_norm

    # create D penalization matrix
    if (lop$penalty == 'lasso'){
      D1 <- cbind(diag(lop$rho,nscans),diag(0,nscans))
      D2 <- cbind(diag(0,nscans),diag((1-lop$rho),nscans))
      D <- rbind(D1,D2)
      if (lop$rho == 0) {D <- D2}
      if (lop$rho == 1) {D <- D1}  # This option doesn't make a lot of sense (penalty only on DS0),
      # just considered in case of a crazy use of the program
      rm(D1); rm(D2)
    }
    infoDeconv$rho <- lop$rho
    infoDeconv$D <- D

  } else {

    # create model matrix for multi-echo formulation with only DR2
    hrf_mtx_ME <- matrix(0,n_inputs*nscans,nscans)
    for (iter_input in 1:n_inputs){
      if (lop$block == TRUE) {
        hrf_mtx_ME[(((iter_input-1)*nscans)+1):(iter_input*nscans),(1:nscans)] = -(lop$TEs[iter_input]/mean(lop$TEs))*hrf_mtx %*% lower.tri(hrf_mtx, diag=TRUE)
      } else {
        hrf_mtx_ME[(((iter_input-1)*nscans)+1):(iter_input*nscans),(1:nscans)] = -(lop$TEs[iter_input]/mean(lop$TEs))*hrf_mtx
      }
    }

    infoDeconv$hrf_mtx_ME <- hrf_mtx_ME
    # normalize columns to unit norm
    hrf_mtx_ME_norm = matrix(0,n_inputs*nscans,nscans)
    norm_hrf_mtx_ME <- sqrt(diag(t(hrf_mtx_ME) %*% hrf_mtx_ME))
    # also keep track coefficients where unit-norm normalization was applied (i.e. norm > 0)
    idx2normalize <- which(norm_hrf_mtx_ME > 0)
    hrf_mtx_ME_norm[,idx2normalize] <- hrf_mtx_ME[,idx2normalize] %*% diag(1/norm_hrf_mtx_ME[idx2normalize])
    infoDeconv$idx2normalize <- idx2normalize
    infoDeconv$hrf_mtx_ME_norm <- hrf_mtx_ME_norm

    infoDeconv$rho <- 0
    infoDeconv$D <- diag(1,nscans)

  }
}

# # -idx_hrf option given, voxel-dependent HRF but indicated with volume of indexes
# # First check that -hrf was input, if -idx_hrf is also input
# if ( (is.null(lop$hrf)) && (!is.null(lop$idx_hrf)) ) {
#   errex.AFNI(sprintf("No -hrf .1D file given with -idx_hrf. You must provide HRF functions. Otherwise use only -hrf.\n"))
# }
# if ( (!is.null(lop$hrf)) && (!is.null(lop$idx_hrf)) ) {
#
#   if (!(substr(lop$hrf,nchar(lop$hrf)-2,nchar(lop$hrf))==".1D") &&
#         !(substr(lop$hrf,nchar(lop$hrf)-4,nchar(lop$hrf))==".1D\'")) {
#     errex.AFNI(sprintf("With -idx_hrf, the -hrf file must be .1D file.\n"))
#   } else {
#     # check if index volume has the same dimensions as input volume
#     temp <- read.AFNI(lop$idx_hrf, verb = max(0,lop$verb-1), meth=lop$iometh)
#     itemp <-  dset.3DBRKarrayto1D(temp)
#     if (!dset.gridmatch(itemp, idset)) {
#       errex.AFNI(sprintf("Mismatch between grids of idx_hrf %s and input %s",lop$idx_hrf, lop$input[1]))
#     }
#     if (NCOL(itemp$brk) > 1) {
#       errex.AFNI(sprintf("Only one index is allowed per voxel in %s",lop$idx_hrf))
#     }
#     if (!is.null(mdset)){
#       if (!is.null(lop$mask)){
#         if (lop$verb) {
#           cat(sprintf("Applying mask %s to %s \n", lop$mask,lop$idx_hrf));
#         }
#       }
#       itemp$brk <- mdset$brk*itemp$brk
#     }
#
#     idx_hrf <- itemp
#
#     # read matrix with HRFs
#     if ((substr(lop$hrf,nchar(lop$hrf)-2,nchar(lop$hrf))==".1D") ||
#           (substr(lop$hrf,nchar(lop$hrf)-3,nchar(lop$hrf))==".1D\'")) {
#       hrf_TR <- read.AFNI.matrix(lop$hrf)
#
#       # check that the maximum idx is lower or equal that the number of HRF provided
#       if ((max(idx_hrf$brk)) > NCOL(hrf_TR)) {
#         errex.AFNI(sprintf("Maximum index of HRF is higher than number of HRFs provided in -hrf file.\n"))
#       }
#
#       L_hrf <- NROW(hrf_TR)
#       hrf_mtx <- array(0,dim=c(nscans,nscans+L_hrf-1,NCOL(hrf_TR)))
#       for (i in 1:NCOL(hrf_TR)){
#         hrf_TR_i <- hrf_TR[,i,drop=FALSE]
#         abs_hrf_TR_i <- abs(hrf_TR_i)
#         idx_zeros_hrf <- which(abs_hrf_TR_i < 10*.Machine$double.eps)
#         if (length(idx_zeros_hrf)>0) {
#           cat(sprintf("Warning: Found zero values in HRF %d. Changed to minimum double precision for numerical stability.\n",i))
#           min_hrf <- max(1e-9*min(abs_hrf_TR_i[idx_zeros_hrf]),10*.Machine$double.eps)
#           hrf_TR_i[idx_zeros_hrf] <- min_hrf * sign(hrf_TR_i[idx_zeros_hrf])
#         }
#         # normalize to unit peak
#         hrf_TR_i <- hrf_TR_i / max(abs_hrf_TR_i)
#         # create Toeplitz matrix
#         hrf_mtx_i <- matrix(0,nscans+2*(L_hrf-1),nscans+L_hrf-1)
#         for(k in 1:(nscans+L_hrf-1)) {
#           hrf_mtx_i[k:(k+L_hrf-1),k] <- hrf_TR_i
#         }
#         hrf_mtx_i <- hrf_mtx_i[L_hrf:(nscans+L_hrf-1),1:(nscans+L_hrf-1)]
#         hrf_mtx[,,i] <- hrf_mtx_i
#       }
#       # update infoLHS
#       infoDeconv$HRFvoxeldependent <- 1
#       infoDeconv$HRFvoxelidx <- 1 # indicates that HRF is already created
#       infoDeconv$hrf_mtx <- hrf_mtx
#       infoDeconv$L_hrf <- L_hrf
#     }
#   }
# }
#
# # -hrf_vol option given, voxel-dependent HRF and different for each voxel
# if (!is.null(lop$vol_hrfs)) {
#
#   # check if hrf_vol has the same dimensions as input volume
#   temp <- read.AFNI(lop$vol_hrfs, verb = max(0,lop$verb-1), meth=lop$iometh)
#   itemp <-  dset.3DBRKarrayto1D(temp)
#   # check if TR of hrf_vol is the same as TR of input volume
#   if ( dset.attr(idset,'TR') != dset.attr(temp,'TR') ) {
#     errex.AFNI(sprintf("TR of hrf_vol %s must be the same as TR of input %s",
#                        lop$vol_hrfs, lop$input[1]))
#   }
#   if (!dset.gridmatch(itemp, idset)) {
#     errex.AFNI(sprintf("Mismatch between grids of hrf_vol %s and input %s",
#                        lop$vol_hrfs, lop$input[1]))
#   }
#
#   if (!is.null(mdset)){
#     if (!is.null(lop$mask)) {
#       if (lop$verb) {
#         cat(sprintf("Applying mask %s to %s \n", lop$mask,lop$vol_hrfs));
#       }
#     }
#     L_hrf <- NCOL(itemp$brk)
#     itemp$brk <- (mdset$brk%*%matrix(1,1,L_hrf))*itemp$brk
#   }
#
#   hrf_vol <- itemp
#
#   hrf_mtx <- matrix(0,nscans,nscans)  # this will not be used, it is just to give something
#                                       # in hrf_mtx input in apply / parapply
#
#   # update infoLHS
#   infoDeconv$HRFvoxeldependent <- 1
#   infoDeconv$HRFvoxelidx <- 0  # indicate that HRF must be created
#
#   L_hrf <- NCOL(hrf_vol$brk)
#   infoDeconv$L_hrf <- L_hrf
#   infoDeconv$hrf_mtx <- hrf_mtx
#
# }

###########################################################################################
# Set time points that will be considered for debiasing
# It can be all time points (i.e. n_inputs*nscans)
times_for_debias <- 1:(nscans*n_inputs)
# or only those time points of the input dataset indicated with variable debias_TEopt
if ((!is.null(lop$debias_TEopt)) && (lop$debias_TEopt > 0)){
  times_for_debias <- (((lop$debias_TEopt-1)*nscans)+1):(lop$debias_TEopt*nscans)
}
infoDeconv$times_for_debias <- times_for_debias

###########################################################################################
# Set value of maxiter, maximum number of iterations of homotopy procedure
maxiter <- 2*nscans
if (!is.null(lop$maxiterfactor)){
  maxiter <- floor(2*nscans*lop$maxiterfactor)
}
infoDeconv$maxiter <- maxiter

###########################################################################################
# Set rest of parameters that are required for the algorithm in infoDeconv
infoDeconv$TEs <- lop$TEs
infoDeconv$nscans <- nscans
infoDeconv$criteria <- lop$criteria
infoDeconv$penalty <- lop$penalty
infoDeconv$do_prior_debias <- lop$do_prior_debias
infoDeconv$R2only <- lop$R2only
infoDeconv$n_selection_Nscans <- lop$n_selection_Nscans
infoDeconv$factor_MAD <- lop$factor_MAD
infoDeconv$factor_min_lambda <- lop$factor_min_lambda

###########################################################################################
#Generate names for output datasets if options outALL or outZAll are used
prefix = ""
if (!is.null(lop$prefix)) prefix = lop$prefix
names_out_ALL <- c('DR2','DS0','sigmas_MAD','lambda','costs')
for (ii in 1:length(names_out_ALL)){
  temp <- paste(names_out_ALL[ii],"_pfx",sep="")
  if (prefix == ""){
    lop[[temp]] <- pprefix.AFNI.name(paste(names_out_ALL[ii],lop$datasets[1],sep="_"))
  } else {
    lop[[temp]] <- pprefix.AFNI.name(paste(names_out_ALL[ii],prefix,lop$datasets[1],sep="_"))
  }
}
for (ii in 1:n_inputs){
  temp <- paste('DR2fit',ii,"_pfx",sep="")
  if (prefix == ""){
    lop[[temp]] <- pprefix.AFNI.name(paste('DR2fit',lop$datasets[ii],sep="_"))
  } else {
    lop[[temp]] <- pprefix.AFNI.name(paste('DR2fit',prefix,lop$datasets[ii],sep="_"))
  }
}

###########################################################################################
# create inData = variable with voxel-dependent time-series to pass to Rprog.GenPFM
NoFiles = 1
# if ( (!is.null(lop$idx_hrf)) || (!is.null(lop$vol_hrfs)) ) {
#   NoFiles <- NoFiles +1
# }
inData <- array(0,dim=c(nvoxels,nscans*n_inputs,NoFiles))
inData[ , ,1] <- idsets$brk

# # Also append voxel-dependent parameters for HRF
# if (!is.null(lop$idx_hrf)) {
#   inData[,1,NoFiles] <- idx_hrf$brk
# }
# if (!is.null(lop$vol_hrfs)){
#   inData[,1:L_hrf,NoFiles] <- hrf_vol$brk
# }

###########################################################################################
# prepare data for parallel computation
nSeg <- lop$nSeg
# break into 20 segments, leading to 5% increamental in parallel computing
dimSeg <- nvoxels%/%nSeg + 1
# number of voxels need to be filled
fill <- nSeg-nvoxels%%nSeg

# Input data and Voxel Dependent regressors
# pad data structure with extra 0s so that all segments have equal number of voxels
inData <- abind(inData, array(0, dim=c(fill,nscans*n_inputs,NoFiles)),along=1)
# break input multiple segments
dim(inData) <- c(dimSeg, nSeg, nscans*n_inputs, NoFiles)
# declare output receiver
out <- array(list(), dim=c(dimSeg, nSeg))

###########################################################################################
# Loop for parallel computation
if (lop$nNodes==1) {
  cat(sprintf("Starting computation of Multiecho Paradigm Free Mapping algorithm.\n"))
  for(kk in 1:nSeg) {
    if (kk > 0) {
      #Run Rprog.MEPFM in this segment
      out[,kk] <- apply(inData[,kk,,,drop=FALSE], 1, Rprog.MEPFM, infoDeconv = infoDeconv)
    }
    if (lop$verb)
      cat("Computation done: ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='');
  }
}

if (lop$nNodes>1) {
  library(snow)
  # open cluster connection
  cat('Opening cluster connection with', lop$nNodes, 'nodes.\n')
  cl <- makeCluster(lop$nNodes, type = "SOCK")
  # make cluster aware of libraries
  clusterEvalQ(cl, library('lars'))
  clusterEvalQ(cl, library('genlasso'))
  clusterEvalQ(cl,library('wavethresh'))
  clusterExport(cl, c("infoDeconv"), envir=environment())
  cat(sprintf("Starting computation of Multiecho Paradigm Free Mapping algorithm.\n"))
  for(kk in 1:nSeg) {
    if (kk > 0) {
      #Run Rprog.MEPFM in this segment
      out[,kk] <- parApply(cl,inData[,kk,,], 1, Rprog.MEPFM, infoDeconv = infoDeconv)
    }
    if (lop$verb)
      cat("Computation done: ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='');
  }
  cat('Closing cluster connection with',lop$nNodes, 'nodes.\n')
  stopCluster(cl)
}

###########################################################################################
# convert to number of original voxels
length(out) <- c(dimSeg*nSeg)
# remove the padded voxels
out <- out[-c((dimSeg*nSeg-fill+1):(dimSeg*nSeg)),drop=F]
# Loop over output variables of Rprog.GenPFM to save in output volumes
names_out <- names(out[[1]])
fields_out <- names(out[[1]])
for (ii in 1:length(names_out)){
  assign(names_out[ii],do.call(rbind,lapply(out,"[[",fields_out[ii])))
}

###########################################################################################

# Reshape output volumes back to 3D mode
dim(DR2) <- ddi
dim(DS0) <- ddi
dim(lambda) <- c(ddi[1:3],1)
dim(costs) <- c(ddi[1:3],infoDeconv$maxiter)
dim(sigmas_MAD) <- c(ddi[1:3],n_inputs)
for (ii in 1:n_inputs){
  temp_data <- eval(as.symbol(paste('DR2fit',ii,sep="")))
  dim(temp_data) <- ddi
  assign(paste('DR2fit',ii,sep=""),temp_data)
}
rm(temp_data)

if (lop$verb) cat( paste('Writing results.\n'));

# writing DR2
write.AFNI(lop$DR2_pfx, DR2, defhead=idset_master, com_hist = lop$com_history);

# writing DS0 (only if R2only is false. Otherwise, DS0 is all zeros)
if (lop$R2only == FALSE) {
  write.AFNI(lop$DS0_pfx, DS0, defhead=idset_master, com_hist = lop$com_history);
}

# writing DR2fit for each input dataset
for (ii in 1:n_inputs){
  temp_pfx <- paste('DR2fit',ii,"_pfx",sep="")
  temp_data <- as.symbol(paste('DR2fit',ii,sep=""))
  write.AFNI(lop[[temp_pfx]], eval(temp_data), defhead=idset_master, com_hist = lop$com_history);
}

# writing lambda
idset_new <- idset_master
dataset_rank_idset_new <- dset.attr(idset_master,"DATASET_RANK")
dataset_rank_idset_new[2] <- 1
idset_new <- dset.attr(idset_new,"DATASET_RANK",val=dataset_rank_idset_new)
taxis_nums_idset_new <- dset.attr(idset_master,"TAXIS_NUMS")
taxis_nums_idset_new[1] <- 1
idset_new <- dset.attr(idset_new,"TAXIS_NUMS",val=taxis_nums_idset_new)
write.AFNI(lop$lambda_pfx, lambda, defhead=idset_new, com_hist = lop$com_history)

# writing costs
idset_new <- idset_master
dataset_rank_idset_new <- dset.attr(idset_master,"DATASET_RANK")
dataset_rank_idset_new[2] <- maxiter
idset_new <- dset.attr(idset_new,"DATASET_RANK",val=dataset_rank_idset_new)
taxis_nums_idset_new <- dset.attr(idset_master,"TAXIS_NUMS")
taxis_nums_idset_new[1] <- maxiter
idset_new <- dset.attr(idset_new,"TAXIS_NUMS",val=taxis_nums_idset_new)
write.AFNI(lop$costs_pfx, costs, defhead=idset_new, com_hist = lop$com_history)

# writing sigmas_MAD
idset_new <- idset_master
dataset_rank_idset_new <- dset.attr(idset_master,"DATASET_RANK")
dataset_rank_idset_new[2] <- n_inputs
idset_new <- dset.attr(idset_new,"DATASET_RANK",val=dataset_rank_idset_new)
taxis_nums_idset_new <- dset.attr(idset_master,"TAXIS_NUMS")
taxis_nums_idset_new[1] <- n_inputs
idset_new <- dset.attr(idset_new,"TAXIS_NUMS",val=taxis_nums_idset_new)
write.AFNI(lop$sigmas_MAD_pfx, sigmas_MAD, defhead=idset_new, com_hist = lop$com_history)
