********
3dZcutup
********

.. _ahelp_3dZcutup:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dZcutup [options] dataset
    Cuts slices off a dataset in its z-direction, and writes a new
    dataset.  The z-direction and number of slices in a dataset
    can be determined using the 3dinfo program.
    Options:
     -keep b t   = Keep slices numbered 'b' through 't', inclusive.
                     This is a mandatory option.  If you want to
                     create a single-slice dataset, this is allowed,
                     but AFNI may not display such datasets properly.
                     A single slice dataset would have b=t.  Slice
                     numbers start at 0.
     -prefix ppp = Write result into dataset with prefix 'ppp'
                     [default = 'zcutup']
    Notes:
     * You can use a sub-brick selector on the input dataset.
     * 3dZcutup won't overwrite an existing dataset (I hope).
     * This program is adapted from 3dZeropad, which does the
         same thing, but along all 3 axes.
     * You can glue datasets back together in the z-direction
         using program 3dZcat.  A sample C shell script that
         uses these progams to carry out an analysis of a large
         dataset is:
    
      #!/bin/csh
      # Cut 3D+time dataset epi07+orig into individual slices
    
      foreach sl ( `count -dig 2 0 20` )
        3dZcutup -prefix zcut${sl} -keep $sl $sl epi07+orig
    
        # Analyze this slice with 3dDeconvolve separately
    
        3dDeconvolve -input zcut${sl}+orig.HEAD            \
                     -num_stimts 3                         \
                     -stim_file 1 ann_response_07.1D       \
                     -stim_file 2 antiann_response_07.1D   \
                     -stim_file 3 righthand_response_07.1D \
                     -stim_label 1 annulus                 \
                     -stim_label 2 antiann                 \
                     -stim_label 3 motor                   \
                     -stim_minlag 1 0  -stim_maxlag 1 0    \
                     -stim_minlag 2 0  -stim_maxlag 2 0    \
                     -stim_minlag 3 0  -stim_maxlag 3 0    \
                     -fitts zcut${sl}_fitts                \
                     -fout -bucket zcut${sl}_stats
      end
    
      # Assemble slicewise outputs into final datasets
    
      time 3dZcat -verb -prefix zc07a_fitts zcut??_fitts+orig.HEAD
      time 3dZcat -verb -prefix zc07a_stats zcut??_stats+orig.HEAD
    
      # Remove individual slice datasets
    
      /bin/rm -f zcut*
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
