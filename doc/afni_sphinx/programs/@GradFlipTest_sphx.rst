*************
@GradFlipTest
*************

.. _@GradFlipTest:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    #------------------------------------------------------------------------
    
        Simple script to test what 'flip', if any, should likely be
        performed for a data set when using 1dDW_Grad_o_Mat++.
    
        **Majorly updated in Jan, 2017-- otherwise you wouldn't even be
          reading this help file description!**
    
        When using this function and looking at the number of tracts per
        flip, there should be a *very* clear winner.  If there isn't, then
        probably something is not correct in the data (something
        inconsistent in bvals or bvecs, large noise, etc.).  Please make
        sure to look at the results in SUMA when prompted at the end, to
        make sure that everything makes sense!
    
        ver 2.93; revision date Oct 12, 2017.
        Written by PA Taylor (NIH).
       
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
      OUTPUT 
    
        On a good day, this function will:
    
        + Recommend using either '-no_flip', or one
          of the {-flip_x|-flip_y|-flip_z} options for 1dDW_Grad_o_Mat++.
    
        + It will store this snippet of code in a file called 
          (default name), which the User could be used in scripting later.
    
        + It will produce a temporary working directory called
          '_tmp_TESTFLIP/' to store intermediate files, of which there are
          many (could be wiped away with '-do_clean').  
    
        + It will also prompt you, O User, to visually check the
          tract results with some simple example scripts (some day it might
          automatically make snapshots!).
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
      COMMAND
    
        @GradFlipTest  \
        -in_dwi     DWI                                                 \
        { -in_row_vec | -in_col_vec | -in_col_matA | -in_col_matT } FF  \
        { -mask MASK }                                                  \
        { -in_bvals BB }                                                \
        { -alg_Thresh_FA X }                                            \
        { -alg_Thresh_Len L }                                           \
        { -prefix     PPP }                                             \
        { -scale_out_1000 }                                             \
        { -wdir WWW }                                                   \
        { -do_clean }
    
      USAGE
        (*must* input 1 set of DWIs *and* 1 set of grads-- choice of format):
    
        -in_dwi     DWI :set of DWIs (N total volumes)
        -in_row_vec  FF :set of row-wise gradient vectors
        -in_col_vec  FF :set of column-wise gradient vectors
        -in_col_matA FF :set of column-wise g- or b-matrix elements
                         ("AFNI"-style format, "diagonal-first")
        -in_col_matT FF :set of column-wise g- or b-matrix elements
                         ("TORTOISE"-style format, "row-first")
    
        -mask      MASK :option mask (probably whole brain); otherwise,
                         automasking is performed 
        -in_bvals    BB :can input bvals, as in 1dDW_Grad_o_Mat++, if 
                         necessary (but shouldn't be necessary?)
    
     -alg_Thresh_FA   X :set minimum FA value for tracking (default X=0.2
                         as for adult, healthy WM parenchyma)
     -alg_Thresh_Len  L :set minimum tract length to require to keep a tract
                         when propagating (default L=30mm ; probably want it
                         to be a bit on the longside for clear counting and
                         comparison)
    
        -prefix     PPP :output name of text file that stores recommended
                         flip opt (default is ).  This option is now
                         also used to determine the directory for all outputs
                         of this program, via the path of PPP.
            NB: The previous, separate option for specifying output directory 
                was '-outdir OUT', but this no longer is used;  the path of an
                output directory is specified by taking the path-part of the 
                '-prefix PPP' input.
    
        -scale_out_1000 :as in 3dDWItoDT.  Probably not necessary, since we 
                         are just checking out trackability
    
        -wdir WWW       :rename working directory output; useful if running 
                         multiple iterations.  Default: _tmp_TESTFLIP. 
                         NB: WWW should *only* be the name of the directory,
                         not contain path info-- the location of WWW is just
                         determined by the path for output, which comes from
                         the path part of PPP/
    
        -do_clean       :remove temporary directory
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
      EXAMPLES
    
        @GradFlipTest  \
            -in_dwi       DWI.nii.gz          \
            -in_col_matA  BMTXT_AFNI.txt
    
        or (perhaps if scanning infants, who have less developed myelin)
    
        @GradFlipTest  \
            -in_dwi        DWI.nii.gz         \
            -in_col_vec    GRADS.txt          \
            -mask          mask_DWI.nii.gz    \
            -alg_Thresh_FA 0.1
    
