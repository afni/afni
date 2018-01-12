.. contents:: 
    :depth: 4 

***************
1dDW_Grad_o_Mat
***************

.. code-block:: none

      
    Simple function to manipulate DW gradient vector files, b-value
    files, and b-/g-matrices. Let: g_i be one of Ng spatial gradients
    in three dimensions; the g-matrix is G_{ij} = g_i*g_j (i.e., dyad
    of gradients, without b-value included); and the DW-scaled
    b-matrix is B_{ij} = b*g_i*g_j.
    
      **NB: please consider using the newer function '1dDW_Grad_o_Mat++'
            instead of this one, as modern thinking means much of the
            defaults (such as averaging reference b0 volumes together
            and functionality here is not really in vogue anymore.
            At some point, the present program will go the way of the
            Silesauridae.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
    As of right now, one can input:
      + 3 rows of gradients (as output from dcm2nii, for example);
      + 3 columns of gradients;
      + 6 columns of g- or b-matrices, in `diagonal-first' order:
             Bxx, Byy, Bzz, Bxy, Bxz, Byz,
        which is used in 3dDWItoDT, for example;
      + 6 columns of g- or b-matrices, in `row-first' order:
             Bxx, 2*Bxy, 2*Bxz, Byy, 2*Byz, Bzz, 
        which is output by TORTOISE, for example;
    
      + when specifying input file, one can use the brackets '{ }'
        in order to specify a subset of rows to keep (NB: probably
        can't use this grad-filter when reading in row-data right
        now).
    During processing, one can:
      + flip the sign of any of the x-, y- or z-components, which
        may be necessary to do to make the scanned data and tracking
        work happily together;
      + filter out all `zero' rows of recorded reference images;
      
    One can then output:
      + 3 columns of gradients;
      + 6 columns of g- or b-matrices, in 'diagonal-first' order;
      + 6 columns of g- or b-matrices, in 'row-first' order;
      + as well as including a column of b-values (such as used in;
        DTI-Studio);
      + as well as including a row of zeros at the top;
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
     + RUNNING:
        1dDW_Grad_o_Mat                                                 \
             { -in_grad_cols  | -in_grad_cols_bwt |                     \
               -in_gmatT_cols | -in_gmatA_cols |                        \
               -in_bmatT_cols | -in_gmatA_cols |                        \
               -in_grad_rows }  INFILE                                  \
             { -flip_x | -flip_y | -flip_z }                            \
             { -keep_b0s } { -put_zeros_top } { -out_bval_col }         \
             { -bref_mean_top }                                         \
             { -in_bvals BVAL_IN }                                      \
             { -bmax_ref THRESH }                                       \
             { -out_grad_cols  | -out_grad_cols_bwt |                   \
               -out_gmatT_cols | -out_gmatA_cols |                      \
               -out_bmatT_cols | -out_gmatA_cols |                      \
               -out_grad_rows }  OUTFILE                                \
             { -out_bval_row_sep | -out_bval_col_sep BB }               
    
        where:
            (one of the following six formats of input must be given):
        -in_grad_rows  INFILE :input file of 3 rows of gradients (e.g., dcm2nii-
                               format output).
        -in_grad_cols  INFILE :input file of 3 columns of gradients.  
     -in_grad_cols_bwt INFILE :input file of 3 columns of gradients, each
                               weighted by the b-value.
        -in_gmatA_cols INFILE :input file of 6 columns of g-matrix in 'A(FNI)'
                               `diagonal first'-format. (See above.)
        -in_gmatT_cols INFILE :input file of 6 columns of g-matr in 'T(ORTOISE)'
                               `row first'-format. (See above.)
        -in_bmatA_cols INFILE :input file of 6 columns of b-matrix in 'A(FNI)'
                               `diagonal first'-format. (See above.)
        -in_bmatT_cols INFILE :input file of 6 columns of b-matr in 'T(ORTOISE)'
                               `row first'-format. (See above.)
            (one of the following five formats of output must be given):
    
       -out_grad_cols OUTFILE :output file of 3 columns of gradients.  
     -out_grad_cols_bwt OUTFILE :output file of 3 columns of gradients, each  
                               weighted by the b-value.
      -out_gmatA_cols OUTFILE :output file of 6 columns of g-matrix in 'A(FNI)'
                               `diagonal first'-format. (See above.)
      -out_gmatT_cols OUTFILE :output file of 6 cols of g-matr in 'T(ORTOISE)'
                               `row first'-format. (See above.)
      -out_bmatA_cols OUTFILE :output file of 6 columns of b-matrix in 'A(FNI)'
                               `diagonal first'-format. (See above.)
      -out_bmatT_cols OUTFILE :output file of 6 cols of b-matr in 'T(ORTOISE)'
                              `row first'-format. (See above.)
      -out_grad_rows  OUTFILE :output file of 3 rows of gradients.
    
            (and any of the following options may be used):
        -proc_dset    DSET    :input a dataset DSET of X 'b=0' and Y DWI bricks,
                               matching the X zero- and Y nonzero-gradient 
                               entries in the INFILE. The 'processing' will:
                                      1) extract all the 'b=0' bricks,
                                      2) average them,
                                      3) store the result in the zeroth brick of
                                         the output PREFIX data set, and
                                      4) place the DWIs (kept in their original
                                         order) as the next Y bricks of PREFIX.
                               This option cannot be used with '-keep_b0s'.
                               The output set has Y+1 bricks.  The option is
                               probably mostly useful only if X>1.
        -pref_dset    PREFIX  :output dataset filename prefix (required and iff
                               using '-proc_dset', above).
        -dwi_comp_fac N_REP   :option for averaging DWI bricks in DSET that have
                               been acquired with exactly N_REP repeated sets of
                               gradients. *You* the user must know how many
                               repetitions have been performed (this program
                               will perform a simplistic gradient comparison
                               using dot products to flag possible errors, but
                               this is by no means bulletproof.  Use wisely.
    
        -flip_x               :change sign of first column of gradients
        -flip_y               :change sign of second column of gradients
        -flip_z               :change sign of third column of gradients
    
        -bref_mean_top        :when averaging the reference X 'b0' values (which
                               is default behavior), have the mean of the X 
                               values be represented in the top row; default 
                               behavior is to have nothing representing the b0
                               information in the top row (for historical
                               functionality reasons).  NB: if your reference
                               'b0' actually has b>0, you might not want to 
                               average the b0 refs together, because their
                               images could have differing contrast if the
                               same reference vector wasn't used for each.
        -keep_b0s             :default function is to get rid of all reference
                               image, but this option acts as switch to keep
                               them.
        -put_zeros_top        :whatever the output format is, add a row at the
                               top with all zeros.
        -bmax_ref THRESH      :THRESH is a scalar number below which b-values
                               (in BVAL_IN) are considered `zero' or reference.
                               Sometimes, for the reference images, the scanner
                               has a value like b=5 s/mm^2, instead of strictly
                               b=0 strictly. One can still flag such values as
                               being associated with a reference image and
                               trim it out, using, for the example case here, 
                               '-bmax_ref 5.1'.
    
        -in_bvals BVAL_IN     :BVAL_IN is a file of b-values, such as the 'bval'
                               file generated by dcm2nii.
        -out_bval_col         :switch to put a column of the bvalues as the
                               first column in the output data.
        -out_bval_row_sep BB  :output a file BB of bvalues in a single row.
        -out_bval_col_sep BB  :output a file BB of bvalues in a single row.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
