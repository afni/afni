*********
3dNetCorr
*********

.. _3dNetCorr:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
      Calculate correlation matrix of a set of ROIs (using mean time series of
      each). Several networks may be analyzed simultaneously, one per brick.
    
      Written by PA Taylor (March, 2013), part of FATCAT (Taylor & Saad,
      2013) in AFNI.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
      + USAGE: Input a set of 4D data and a set of ROI masks (i.e., a bunch of 
             ROIs in a brik each labelled with a distinct integer), and get a
             matrix of correlation values for it.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 3dNetCorr -prefix PREFIX {-mask MASK} {-fish_z} {-part_corr} \
                    -inset FILE -in_rois INROIS {-ts_out} {-ts_label}         \
                    {-ts_indiv} {-ts_wb_corr} {-ts_wb_Z} {-nifti}             \
                    {-push_thru_many_zeros} {-ts_wb_strlabel}                 \
                    {-output_mask_nonnull}
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + OUTPUT: 
            Output will be a simple text file, first with the number N of ROIs
            in the set, then an empty line, then a list of the ROI labels in the
            file (i.e., col/row labels), empty line, and then an NxN matrix of
            correlation values (diagonals should be unity). One can also output
            the Fisher Z-transform of the matrix (with zeros along diag).
            If multiple subbricks are entered, one gets multiple files output,
            one per subbrick/network.
            Naming convention of outputs: PREFIX_???.netcc, where `???'
            represents a zero-padded version of the network number, based on the
            number of subbricks in the `in_rois' option (i.e., 000, 001,...).
            If the `-ts_out' option is used, the mean time series per ROI, one
            line, are output in PREFIX_???.netts files.
            Labeltables are now also supported; when an '-inset FILE' contains
            a labeltable, the labels will then be passed to the *.netcc file.
            These labels may then be referred to in plotting/output, such as
            using fat_mat_sel.py.
            +NEW+ (Dec. 2014): A PREFIX_???.niml.dset is now also output
            automatically.  This NIML/SUMA-esque file is mainly for use in SUMA,
            for visualizing connectivity matrix info in a 3D brain.  It can be
            opened via, for example:
            $ suma -vol ROI_FILE  -gdset FILE.niml.dset
    
            It is now also possible to output whole brain correlation maps,
            generated from the average time series of each ROI,
            as either Pearson r or Fisher-transformed Z-scores (or both); see
            the '-ts_wb*' options below.
    
            [As of April, 2017] There is now more checking done for having any
            null time series in ROIs.  They are bad to have around, esp. when
            they fill an ROI.  A new file called 'PREFIX.roidat' is now output,
            whose columns contain information for each ROI in the used mask:
            [Nvox] [Nvox with non-null ts] [non-null frac] # [ROI number] [label]
            The program also won't run now by default if an ROI contains more
            than 10 percent null time series; one can use a '-push*' option
            (see below) to still calculate anyways, but it will definitely cease
            if any ROI is full of null time series.
            ... And the user can flag to output a binary mask of the non-null
            time series, called 'PREFIX_mask_nnull*', with the new option
            '-output_mask_nonnull'.  This might be useful to check if your data
            are well-masked, if you haven't done so already (and you know who
            you are...).
    
            [As of April, 2017] On a minor note, one can also apply string labels
            to the WB correlation/Z-score output files;  see the option
            '-ts_wb_strlabel', below.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + RUNNING, need to provide:
        -prefix PREFIX   :output file name part (see description below).
        -inset  FILE     :time series file (4D data set). 
    
        -mask   MASK     :can include a whole brain mask within which to
                          calculate correlation. (Otherwise, data should be
                          masked already; the program will try to analyze.)
        -in_rois INROIS  :can input a set of ROIs, each labelled with distinct
                          integers. Multiple subbricks can be input, each will
                          be treated as a separate network.
        -fish_z          :switch to also output a matrix of Fisher Z-transform
                          values for the corr coefs (r):
                              Z = atanh(r) ,
                          (with Z=4 being output along matrix diagonals where
                          r=1, as the r-to-Z conversion is ceilinged at 
                          Z = atanh(r=0.999329) = 4, which is still *quite* a
                          high Pearson-r value.
        -part_corr       :output the partial correlation matrix. It is 
                          calculated from the inverse of regular Pearson
                          matrix, R, as follows: let M = R^{I} be in the inverse
                          of the Pearson cc matrix.  Then each element p_{ij} of
                          the partial correlation (PC) matrix is given as:
                          p_{ij} = -M_{ij}/sqrt( M_{ii} * M_{jj} ).
                          This will also calculate the PC-beta (PCB) matrix,
                          which is not symmetric, and whose values are given as:
                          b_{ij} = -M_{ij}/M_{ii}.
                          Use as you wish.  For both PC and PCB, the diagonals
                          should be uniformly (negative) unity.
        -ts_out          :switch to output the mean time series of the ROIs that
                          have been used to generate the correlation matrices.
                          Output filenames mirror those of the correlation
                          matrix files, with a '.netts' postfix.
        -ts_label        :additional switch when using '-ts_out'. Using this
                          option will insert the integer ROI label at the start
                          of each line of the *.netts file created. Thus, for
                          a time series of length N, each line will have N+1
                          numbers, where the first is the integer ROI label
                          and the subsequent N are scientific notation values.
        -ts_indiv        :switch to create a directory for each network that
                          contains the average time series for each ROI in
                          individual files (each file has one line).
                          The directories are labelled PREFIX_000_INDIV/,
                          PREFIX_001_INDIV/, etc. (one per network). Within each
                          directory, the files are labelled ROI_001.netts,
                          ROI_002.netts, etc., with the numbers given by the
                          actual ROI integer labels.
        -ts_wb_corr      :switch to perform whole brain correlation for each
                          ROI's average time series; this will automatically
                          create a directory for each network that contains the
                          set of whole brain correlation maps (Pearson 'r's).
                          The directories are labelled as above for '-ts_indiv'
                          Within each directory, the files are labelled
                          WB_CORR_ROI_001+orig, WB_CORR_ROI_002+orig, etc., with
                          the numbers given by the actual ROI integer labels.
        -ts_wb_Z         :same as above in '-ts_wb_corr', except that the maps
                          have been Fisher transformed to Z-scores the relation:
                          Z=atanh(r). 
                          To avoid infinities in the transform, Pearson values 
                          are effectively capped at |r| = 0.999329 (where
                          |Z| = 4.0;  hope that's good enough).
                          Files are labelled WB_Z_ROI_001+orig, etc.
    
        -ts_wb_strlabel  :by default, '-ts_wb_{corr,Z}' output files are named
                          using the int number of a given ROI, such as:
                            WB_Z_ROI_001+orig.
                          with this option, one can replace the int (such as
                          '001') with the string label (such as 'L-thalamus')
                          *if* one has a labeltable attached to the file.
        -nifti           :output any correlation map files as NIFTI files
                          (default is BRIK/HEAD). Only useful if using
                          '-ts_wb_corr' and/or '-ts_wb_Z'.
    
       -output_mask_nonnull
                         :internally, this program checks for where there are
                          nonnull time series, because we don't like those, in
                          general.  With this flag, the user can output the
                          determined mask of non-null time series.
       -push_thru_many_zeros
                         :by default, this program will grind to a halt and
                          refuse to calculate if any ROI contains >10 percent
                          of voxels with null times series (i.e., each point is
                          0), as of April, 2017.  This is because it seems most
                          likely that hidden badness is responsible. However,
                          if the user still wants to carry on the calculation
                          anyways, then this option will allow one to push on
                          through.  However, if any ROI *only* has null time
                          series, then the program will not calculate and the
                          user will really, really, really need to address
                          their masking.
    
        -ignore_LT       :switch to ignore any label table labels in the 
                          '-in_rois' file, if there are any labels attached.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
          3dNetCorr                                  \
             -inset REST_in_DWI.nii.gz               \
             -in_rois ROI_ICMAP_GM+orig              \
             -fish_z                                 \
             -ts_wb_corr                             \
             -mask mask_DWI+orig                     \
             -prefix FMRI/REST_corr
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
