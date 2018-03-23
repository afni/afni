.. _ahelp_3dBrainSync:

***********
3dBrainSync
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  3dBrainSync [options]
    
    This program 'synchronizes' the -inset2 dataset to match the -inset1
    dataset, as much as possible (average voxel-wise correlation), using the
    same transformation on each input time series from -inset2:
    
     ++ With the -Qprefix option, the transformation is an orthogonal matrix,
        computed as described in Joshi's original OHBM 2017 presentations.
    
     ++ With the -Pprefix option, the transformation is simply a
        permutation of the time order of -inset2 (a special case
        of an orthogonal matrix).
    
     ++ The algorithms and a little discussion of the different features of
        these two techniques are discussed in the METHODS section, infra.
    
     ++ At least one of '-Qprefix' or '-Pprefix' must be given, or
        this program does not do anything! You can use both methods,
        if you want to compare them.
    
     ++ 'Harmonize' might be a better name for what this program does,
        but calling it 3dBrainHarm would probably not be good marketing
        (except for Traumatic Brain Injury researchers?).
    
    One possible application of this program is to correlate resting state
    FMRI datasets between subjects, voxel-by-voxel, as is sometimes done
    with naturalistic stimuli (e.g., movie viewing).
    
    --------
    OPTIONS:
    --------
     -inset1 dataset1 = Reference dataset
    
     -inset2 dataset2 = Dataset to be matched to the reference dataset,
                        as much as possible.
                        ++ These 2 datasets must be on the same spatial grid,
                           and must have the same number of time points!
                        ++ There must be at least twice as many voxels being
                           processed as there are time points (see '-mask', below).
                        ++ These are both MANDATORY 'options'.
                        ++ As usual in AFNI, since the computations herein are
                           voxel-wise, it is possible to input plain text .1D
                           files as datasets. When doing so, remember that
                           a ROW in the .1D file is interpreted as a time series
                           (single voxel's data). If your .1D files are oriented
                           so that time runs in down the COLUMNS, you will have to
                           transpose the inputs, which can be done on the command
                           line with the \' operator, or externally using the
                           1dtranspose program.
                    -->>++ These input datasets should be pre-processed first
                           to remove undesirable components (motions, baseline,
                           spikes, breathing, etc). Otherwise, you will be trying
                           to match artifacts between the datasets, which is not
                           likely to be interesting or useful. 3dTproject would be
                           one way to do this. Even better: afni_proc.py!
                        ++ In particular, the mean of each time series should have
                           been removed! Otherwise, the calculations are fairly
                           meaningless.
    
     -Qprefix qqq     = Specifies the output dataset to be used for
                        the orthogonal matrix transformation.
                        ++ This will be the -inset2 dataset transformed
                           to be as correlated as possible (in time)
                           with the -inset1 dataset, given the constraint
                           that the transformation applied to each time
                           series is an orthogonal matrix.
    
     -Pprefix ppp     = Specifies the output dataset to be used for
                        the permutation transformation.
                        ++ The output dataset is the -inset2 dataset
                           re-ordered in time, again to make the result
                           as correlated as possible with the -inset1
                           dataset.
    
     -normalize       = Normalize the output dataset(s) so that each
                        time series has sum-of-squares = 1.
                        ++ This option is not usually needed in AFNI
                           (e.g., 3dTcorrelate does not care).
    
     -mask mset       = Only operate on nonzero voxels in the mset dataset.
                        ++ Voxels outside the mask will not be used in computing
                           the transformation, but WILL be transformed for
                           your application and/or edification later.
                        ++ For FMRI purposes, a gray matter mask would make
                           sense here, or at least a brain mask.
                        ++ If no masking option is given, then all voxels
                           will be processed in computing the transformation.
                           This set will include all non-brain voxels (if any).
                        ++ Any voxel which is all constant in time
                           (in either input) will be removed from the mask.
                        ++ This mask dataset must be on the same spatial grid
                           as the other input datasets!
    
     -verb             = Print some progress reports and auxiliary information.
                         ++ Use this option twice to get LOTS of progress
                            reports; mostly useful for debugging.
    
    ------
    NOTES:
    ------
    * Is this program useful? Not even The Shadow knows!
      (But do NOT call it BS.)
    
    * The output dataset is in floating point format.
    
    * Although the goal of 3dBrainSync is to make the transformed
      -inset2 as correlated (voxel-by-voxel) as possible with -inset1,
      it does not actually compute that correlation dataset. You can do
      that computation with program 3dTcorrelate, as in
        3dTcorrelate -polort -1 -prefix AB.pcor.nii \
                     dataset1 transformed-dataset2
    
    * Besides the transformed dataset(s), if the '-verb' option is used,
      some other (text formatted) files are written out:
       {Qprefix}.sval.1D = singular values from the BC' decomposition
       {Qprefix}.qmat.1D = Q matrix
       {Pprefix}.perm.1D = permutation indexes p(i)
      You probably do not have any use for these files; they are mostly
      present to diagnose any problems.
    
    --------
    METHODS:
    --------
    * Notation used in the explanations below:
        M = Number of time points
        N = Number of voxels > M (N = size of mask)
        B = MxN matrix of time series from -inset1
        C = MxN matrix of time series from -inset2
            Both matrices will have each column normalized to
            have sum-of-squares = 1 (L2 normalized) --
            the program does this operation internally; you do not have
            to ensure that the input datasets are so normalized)
        Q = Desired orthgonal MxM matrix to transform C such that B-QC
            is as small as possible (sum-of-squares = Frobenius norm)
            normF(A) = sum_{ij} A_{ij}^2 = trace(AA') = trace(A'A).
            NOTE: This norm is different from the matrix L2 norm.
            NOTE: A' denotes the transpose of A.
    
    * The expansion below shows why the matrix BC' is crucial to the analysis:
        normF(B-QC) = trace( [B-QC][B'-C'Q'] )
                    = trace(BB') + trace(QCC'Q') - trace(BC'Q') - trace(QCB')
                    = trace(BB') + trace(C'C) - 2 trace(BC'Q')
      The second term collapses because trace(AA') = trace(A'A), so
      trace([QC][QC]') = trace([QC]'[QC]) = trace(C'Q'QC) = trace(C'C)
      because Q is orthogonal. So the first 2 terms in the expansion of
      normF(B-QC) do not depend on Q at all. Thus, to minimize normF(B-QC),
      we have to maximize trace(BC'Q') = trace([B][QC]') = trace([QC][B]').
    
      Since the columns of B and C are the (normalized) time series,
      each row represents the image at a particular time. So the (i,j)
      element of BC' is the (spatial) dot product of the i-th TR image from
      -inset1 with the j-th TR image from -inset2. Furthermore,
      trace(BC') = trace(C'B) = sum of dot products (correlations)
      of all time series. So maximizing trace(BC'Q') will maximize the
      summed correlations of B (time series from -inset1) and QC
      (transformed time series from -inset2).
    
      Note again that the sum of correlations (dot products) of all the time
      series is equal to the sum of dot products of all the spatial images.
      So the algorithm to find the transformation Q is to maximize the sum of
      dot products of spatial images from B with Q-transformed spatial images
      from C -- since there are fewer time points than voxels, this is more
      efficient and elegant than trying to maximize the sum over voxels of dot
      products of time series.
    
      If you use the '-verb' option, these summed correlations ('scores')
      are printed to stderr during the analysis.
    
    * Joshi method [-Qprefix]:
        (a) compute MxM matrix B C'
        (b) compute SVD of B C' = U S V' (U, S, V are MxM matrices)
        (c) Q = U V'
            [note: if B=C, then U=V, so Q=I, as it should]
        (d) transform each time series from -inset2 using Q
      This matrix Q is the solution to the restricted least squares
      problem (i.e., restricted to have Q be an orthogonal matrix).
      NOTE: The sum of the singular values in S is equal to the sum
            of the time series dot products (correlations) in B and QC,
            when Q is calculated as above.
    
      A pre-print of this method is available as:
        AA Joshi, M Chong, RM Leahy.
        BrainSync: An Orthogonal Transformation for Synchronization of fMRI
        Data Across Subjects, Proc. MICCAI 2017
      https://www.dropbox.com/s/tu4kuqqlg6r02kt/brainsync_miccai2017.pdf
      https://www.google.com/search?q=joshi+brainsync
      http://neuroimage.usc.edu/neuro/Resources/BrainSync
    
    * Permutation method [-Pprefix]:
        (a) Compute B C' (as above)
        (b) Find a permutation p(i) of the integers {0..M-1} such
            that sum_i { (BC')[i,p(i)] } is as large as possible
            (i.e., p() is used as a permutation of the COLUMNS of BC').
            This permutation is equivalent to post-multiplying BC'
            by an orthogonal matrix P representing the permutation;
            such a P is full of 0s except for a single 1 in each row
            and each column.
        (c) Permute the ROWS (time direction) of the time series matrix
            from -inset2 using p().
      Only an approximate (greedy) algorithm is used to find this
      permutation; that is, the best permutation is not guaranteed to be found
      (just a 'good' permutation -- it is the best thing I could code quickly :).
    
      Algorithm currently implemented (let D=BC' for notational simplicity):
        1) Find the largest element D(i,j) in the matrix.
           Then the permutation at row i is p(i)=j.
           Strike row i and column j out of the matrix D.
        2) Repeat, finding the largest element left, say at D(f,g).
           Then p(f) = g. Strike row f and column g from the matrix.
           Repeat until done.
      (Choosing the largest possible element at each step is what makes this
      method 'greedy'.) This permutation is not optimal but is pretty good,
      and another step is used to improve it:
        3) For all pairs (i,j), p(i) and p(j) are swapped and that permutation
           is tested to see if the trace gets bigger.
        4) This pair-wise swapping is repeated until it does not improve things
           any more (typically, it improves the trace about 1-2% -- not much).
      The purpose of the pair swapping is to deal with situations where D looks
      something like this: [  1 70 ]
                           [ 70 99 ]
      Step 1 would pick out 99, and Step 2 would pick out 1; that is,
      p(2)=2 and then p(1)=1, for a total trace/score of 100. But swapping
      1 and 2 would give a total trace/score of 140. In practice, extreme versions
      of this situation do not seem common with real FMRI data, probably because
      the subject's brain isn't actively conspiring against this algorithm :)
    
      [Something called the 'Hungarian algorithm' can solve for the optimal]
      [permutation exactly, but I've not had the inclination to program it.]
    
      This whole permutation optimization procedure is very fast: about 1 second.
      In the RS-FMRI data I've tried this on, the average time series correlation
      resulting from this optimization is 50-65% of that which comes from
      optimizing over ALL orthogonal matrices (Joshi method). If you use '-verb',
      the stderr output line that looks like this
       + corr scores: original=-722.5 Q matrix=22366.0 permutation=12918.7 57.8%
      shows trace(BC') before any transforms, with the Q matrix transform,
      and with the permutation transform. As explained above, trace(BC') is
      the summed correlations of the time series (since the columns of B and C
      are normalized prior to the optimizations); in this example, the ratio of
      the average time series correlation between the permutation method and the
      Joshi method is about 58% (in a gray matter mask with 72221 voxels).
    
    * Results from the permutation method MUST be less correlated (on average)
      with -inset1 than the Joshi method's results: the permutation can be
      thought of as an orthogonal matrix containing only 1s and 0s, and the BEST
      possible orthogonal matrix, from Joshi's method, has more general entries.
      ++ However, the permutation method has an obvious interpretation
         (re-ordering time points), while the general method linearly combines
         different time points (perhaps far apart); the interpretation of this
         combination in terms of synchronizing brain activity is harder to intuit
         (at least for me).
      ++ Another feature of a permutation-only transformation is that it cannot
         change the sign of data, unlike a general orthgonal matrix; e.g.,
           [ 0 -1]
           [-1  0], which swaps 2 time points AND negates them, is a valid
         orthogonal matrix. For rs-FMRI datasets, this consideration might not
         be important, since correlations are generally positive, so don't often
         need sign-flipping to make them so.
    
    * This program is NOT multi-threaded. Typically, I/O is a big part of
      the run time (at least, for the cases I've tested). The '-verb' option
      will give progress reports with elapsed-time stamps, making it easy to
      see which parts of the program take the most time.
    
    * Author: RWCox, servant of the ChronoSynclastic Infundibulum - July 2017
    
    * Thanks go to Anand Joshi for his clear exposition of BrainSync at OHBM 2017,
      and his encouragement about the development of this program.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
