.. contents:: 
    :depth: 4 

*******
3dMatch
*******

.. code-block:: none

    
      3dMatch, written by PA Taylor (Nov., 2012), part of FATCAT (Taylor & Saad,
        2013) in AFNI.
    
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      Find similar subbricks and rearrange order to ease comparison
    
      Comparison simply done by comparing (weighted) correlation maps of
      values, which may include thresholding of either refset or inset
      values. The weighting is done by squaring each voxel value (whilst
      maintaining its original sign). The Dice coefficient is also calculated
      to quantify overlap of regions.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMANDS: 
        3dMatch -inset FILE1 -refset FILE2 {-mask FILE3} {-in_min THR1} \ 
               {-in_max THR2} {-ref_min THR3} {-ref_max THR4} -prefix FILE4 \
               {-only_dice_thr} 
        where:
         -inset  FILE1  :file with M subbricks of data to match against another
                         file.
         -refset FILE2  :file with N subbricks, serving as a reference for
                         FILE1.  N=M is *not* a requirement; matching is done
                         based on squares of values (with signs preserved), and
                         both best fit of in->ref and ref->in are calculated 
                         and output.
         -mask   FILE3  :a mask of regions to include in the correlation of 
                         data sets; technically not necessary as relative 
                         correlation values shouldn't change, but the magnitudes
                         would scale up without it. Dice coeff values should not
                         be affected by absence or presence of wholebrain mask.
         -in_min  THR1  :during the correlation/matching analysis, values below
                         THR1 in the `-inset' will be zeroed (and during Dice
                         coefficient calculation, excluded from comparison).
                         (See `-only_dice_thr' option, below.)
         -in_max  THR2  :during the correlation/matching analysis, values above
                         THR2 in the `-inset' will be zeroed (and during Dice
                         coefficient calculation, excluded from comparison).
         -ref_min  THR3 :during the correlation/matching analysis, values below
                         THR3 in the `-refset' will be zeroed (and during Dice
                         coefficient calculation, excluded from comparison).
                         (See `-only_dice_thr' option, below.)
         -ref_max  THR4 :during the correlation/matching analysis, values above
                         THR4 in the `-refset' will be zeroed (and during Dice
                         coefficient calculation, excluded from comparison).
         -prefix FILE4  :prefix out output name for both *BRIK/HEAD files, as
                         well as for the *_coeff.vals text files (see below).
         -only_dice_thr :if option is included in command line, the thresholding
                         above is only applied during Dice evaluation, not 
                         during spatial correlation.
    
      + OUTPUTS, named using prefix; 
         *_REF+orig     :AFNI BRIK/HEAD file with the same number of subbricks
                         as the `-refset' file, each one corresponding to a
                         subbrick of the `-inset' file with highest weighted
                         correlation. Any unmatched `-inset' subbricks are NOT
                         appended at the end. (For example, you could underlay
                         the -ref_set FILE2 and visually inspect the comparisons
                         per slice.)
         *_REF_coeff.vals :simple text file with four columns, recording the
                         original brick number slices which have been
                         reordered in the output *_REF+orig file. Cols. 1&2-
                         orig `-refset' and `-inset' indices, respectively;
                         Col. 3- weighted correlation coefficient; Col 4.-
                         simple Dice coefficient.
         *_IN+orig      :AFNI BRIK/HEAD file with the same number of subbricks
                         as the `-inset' file, each one corresponding to
                         a subbrick of the `-refset' file with highest weighted
                         correlation. Any unmatched `-refset' subbricks are NOT
                         appended at the end. (For example, you could underlay
                         the -inset FILE1 and visually inspect the comparisons
                         per slice.)
         *_IN_coeff.vals :simple text file with four columns, recording the
                         original brick number slices which have been
                         reordered in the output *_IN+orig file. Cols. 1&2-
                         orig `-inset' and `-refset' indices, respectively;
                         Col. 3- weighted correlation coefficient; Col 4.-
                         simple Dice coefficient.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
          3dMatch                           \
             -inset CORREL_DATA+orig        \
             -refset STANDARD_RSNs+orig     \
             -mask mask+orig                \
             -in_min 0.4                    \
             -ref_min 2.3                   \
             -prefix MATCHED                \
             -only_dice_thr
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
