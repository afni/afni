*****************
1dDW_Grad_o_Mat++
*****************

.. _1dDW_Grad_o_Mat++:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

      
      Simple function to manipulate DW gradient vector files, b-value
      files, and b- or g-matrices. Let: g_i be one of Ng spatial gradients
      in three dimensions; |g_i| = 1, and the g-matrix is G_{ij} = g_i * g_j
      (i.e., dyad of gradients, without b-value included); and the DW-scaled
      b-matrix is B_{ij} = b * g_i * g_j.
    
      **This new version of the function** will replace the original/older 
      version (1dDW_Grad_o_Mat).  The new has similar functionality, but
      improved defaults:
         + it does not average b=0 volumes together by default;
         + it does not remove top b=0 line from top by default;
         + output has same scaling as input by default (i.e., by bval or not);
           and a switch is used to turn *off* scaling, for unit magn output
           (which is cleverly concealed under the name '-unit_mag_out').
    
      Wherefore, you ask?  Well, times change, and people change.
      The above functionality is still available, but each just requires
      selection with command line switches.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
    As of right now, one can input:
      + 3 rows of gradients (as output from dcm2nii, for example);
      + 3 columns of gradients;
      + 6 columns of g- or b-matrices, in `diagonal-first' (-> matA) order:
             Bxx, Byy, Bzz, Bxy, Bxz, Byz,
        which is used in 3dDWItoDT, for example;
      + 6 columns of g- or b-matrices, in `row-first' (-> matT) order:
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
      + filter out all `zero' rows of recorded reference images, 
        THOUGH this is not really recommended.
      
    One can then output:
      + 3 columns of gradients;
      + 6 columns of g- or b-matrices, in 'diagonal-first' order;
      + 6 columns of g- or b-matrices, in 'row-first' order;
      + as well as including a column of b-values (such as used in, e.g.,
        DSI-Studio);
      + as well as explicitly include a row of zeros at the top;
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
     + RUNNING:
        1dDW_Grad_o_Mat++                                               \
             { -in_row_vec  | -in_col_vec  |                            \
               -in_col_matA | -in_col_matT }  INFILE                    \
             { -flip_x | -flip_y | -flip_z | -no_flip }                 \
             { -out_row_vec  | -out_col_vec  |                          \
               -out_col_matA | -out_col_matT }  OUTFILE                 \
             { -in_bvals BVAL_FILE }                                    \
             { -out_col_bval }                                          \
             { -out_row_bval_sep BB | -out_col_bval_sep BB }            \
             { -unit_mag_out }                                          \
             { -bref_mean_top }                                         \
             { -bmax_ref THRESH }                                       \
             { -put_zeros_top   }                                       \
        where:
            (one of the following formats of input must be given):
        -in_row_vec   INFILE  :input file of 3 rows of gradients (e.g.,
                               dcm2nii-format output).
        -in_col_vec   INFILE  :input file of 3 columns of gradients.  
        -in_col_matA  INFILE  :input file of 6 columns of b- or g-matrix in
                               'A(FNI)' `diagonal first'-format. (See above.)
        -in_col_matT  INFILE  :input file of 6 columns of b- or g-matrix in 
                               'T(ORTOISE)' `row first'-format. (See above.)
    
            (one of the following formats of output must be given):
        -out_row_vec  OUTFILE :output file of 3 rows of gradients.
        -out_col_vec  OUTFILE :output file of 3 columns of gradients.
        -out_col_matA OUTFILE :output file of 6 columns of b- or g-matrix in
                               'A(FNI)' `diagonal first'-format. (See above.)
        -out_col_matT OUTFILE :output file of 6 cols of b- or g-matrix in
                               'T(ORTOISE)' `row first'-format. (See above.)
    
            (and any of the following options may be used):
    
        -in_bvals  BVAL_FILE  :BVAL_FILE is a file of b-values, either a single
                               row (such as the 'bval' file generated by
                               dcm2nii) or a single column of numbers.  Must
                               have the same number of entries as the number
                                of grad vectors or matrices.
        -out_col_bval         :switch to put a column of the bvalues as the
                               first column in the output data.
        -out_row_bval_sep BB  :output a file BB of bvalues in a single row.
        -out_col_bval_sep BB  :output a file BB of bvalues in a single column.
    
        -unit_mag_out         :switch so that each vector/matrix from the INFILE
                               is scaled to either unit or zero magnitude.
                               (Supplementary input bvalues would be ignored
                               in the output matrix/vector, but not in the
                               output bvalues themselves.)  The default
                               behavior of the function is to leave the output
                               scaled however it is input (while also applying
                               any input BVAL_FILE). 
    
        -flip_x               :change sign of first column of gradients (or of
                               the x-component parts of the matrix)
        -flip_y               :change sign of second column of gradients (or of
                               the y-component parts of the matrix)
        -flip_z               :change sign of third column of gradients (or of
                               the z-component parts of the matrix)
        -no_flip              :don't change any gradient/matrix signs.  This
                               is an extraneous switch, as the default is to
                               not flip any signs (this is mainly used for
                               some scripting convenience
    
        -check_abs_min VVV    :By default, this program checks input matrix formats
                               for consistency (having positive semidefinite diagonal
                               matrix elements).  It will fail if those don't occur.
                               However, sometimes there is just a tiny values <0,
                               like a rounding error; you can specify to push through
                               for negative diagonal elements with magnitude <VVV,
                               with those values getting replaced by zero.  Be
                               judicious with this power! (E.g., maybe VVV ~ 0.0001
                               might be OK... but if you get looots of negatives, then
                               you really, really need to check your data for badness.
    
           (and the follow options are probably mainly extraneous, nowadays)
        -bref_mean_top        :when averaging the reference X 'b0' values (the
                               default behavior), have the mean of the X 
                               values be represented in the top row; default 
                               behavior is to have nothing representing the b0
                               information in the top row (for historical
                               functionality reasons).  NB: if your reference
                               'b0' actually has b>0, you might not want to 
                               average the b0 refs together, because their
                               images could have differing contrast if the
                               same reference vector wasn't used for each.
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
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      EXAMPLES
    
       # An example of type-conversion from a TORTOISE-style matrix to column
       # gradients (if the matT file has bweights, so will the grad values):
    
       1dDW_Grad_o_Mat++                                    \
          -in_col_matT   BMTXT_TORT.txt                     \
          -out_col_vec   GRAD.dat                           
    
    
       # An example of filtering (note the different styles of parentheses
       # for the column- and row-type files) and type-conversion (to an
       # AFNI-style matrix that should have the bvalue weights afterwards):
    
       1dDW_Grad_o_Mat++                                    \
          -in_col_vec    GRADS_col.dat'{0..10,12..30}'      \
          -in_bvals      BVALS_row.dat'[0..10,12..30]'      \
          -out_col_matA  FILT_matA.dat                      
    
    
       # An example of filtering *without* type-conversion.  Here, note
       # the '-unit_mag_out' flag is used so that the output row-vec does
       # not carry the bvalue weight with it;  it does not affect the output
       # bval file.  As Levon might say, the '-unit_mag_out' option acts to
       #   'Take a load off bvecs, take a load for free;
       #    Take a load off bvecs, and you put the load right on bvals only.'
       # This example might be useful for working with dcm2nii* output:
    
       1dDW_Grad_o_Mat++                                      \
          -in_row_vec        ap.bvec'[0..10,12..30]'          \
          -in_bvals          ap.bval'[0..10,12..30]'          \
          -out_row_vec       FILT_ap.bvec                     \
          -out_row_bval_sep  FILT_ap.bval                     \
          -unit_mag_out
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
