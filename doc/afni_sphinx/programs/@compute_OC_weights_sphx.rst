*******************
@compute_OC_weights
*******************

.. _ahelp_@compute_OC_weights:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    @compute_OC_weights           - compute optimally combined weights dataset
    
       Given echo times (in a text file) and one run of multi-echo EPI data,
       compute a dataset that can be used to combine the echoes.  The weight
       dataset would have one volume per echo, which can be used to combine
       the echoes into a single dataset.  The same echoes can be applied to
       all runs.
    
        3dcalc -a echo1+tlrc        -b echo2+tlrc        -c echo3+tlrc        \
               -d weights+tlrc'[0]' -e weights+tlrc'[1]' -f weights+tlrc'[2]' \
               -expr 'a*d+b*e+c*f'  -prefix opt.combined
    
       ----------------------------------------------------------------------
    
       These computations are based on the system of equations from the summer
       2017 presentation by Javier Gonzalez-Castillo.  After solving:
      
         log(mean(S(TE_1))) ~= -mean(R2s(x))*TE_1 + log(So(x))
         log(mean(S(TE_2))) ~= -mean(R2s(x))*TE_2 + log(So(x))
         log(mean(S(TE_3))) ~= -mean(R2s(x))*TE_3 + log(So(x))
      
       then T2* = 1/mean(R2s(x)), and weights come from:
      
                      TE_n*e^-(TE_n/T2*)
         w(TE_n) = -------------------------
                   sum_n[TE_n*e^-(TE_n/T2*)]
    
       Bad, naughty voxels are defined as those with either negative T2* values,
       or for which the sum of the weights is not sufficiently close to 1, which
       would probably mean that there were computational truncation errors, likely
       due to R2s being very close to 0.
    
       so "fail" if
             mean(R2s) <= 0
       or
             abs(1-sum[weights]) > 'tolerance'
    
       In such cases, the weights will default to the result based on the maximum
       T2* value (unless "-def_to_equal yes" is applied, in which case the default
       is 1/number_of_echoes, which is equal weighting across echoes).
      
       ----------------------------------------------------------------------
    
       examples:
    
          1. basic
    
             @compute_OC_weights -echo_times_file etimes.1D \
                    -echo_dsets pb02*r01*volreg*.HEAD
      
          2. specify working directory and resulting weights dataset prefix
    
             @compute_OC_weights -echo_times_file etimes.1D \
                    -echo_dsets pb02*r01*volreg*.HEAD       \
                    -prefix OC.run1 -work_dir OC.work.run1
    
       ----------------------------------------------------------------------
    
       random babble:
    
          The T2* map is not actually used, but rather 1/T2* (to avoid repeated
          division).
    
          T2* is restricted to the range (0, T2S_LIMIT), where the default limit is
          300 (see -t2_star_limit).
    
          A "bad" T2* value (T2* <= 0 or T2* > T2S_LIMIT) will lead to use of the
          limit T2S_LIMIT, so that as R2 decreases and goes negative, the results
          converge.
    
          If the sum of the weights is not almost exactly 1.0 (see the option,
          -sum_weight_tolerance), the weights will also default to equal (see
          option -def_to_equal).
    
          Basically, the program is designed such that either a reasonable T2*
          is computed and applied, or the weighting result will be 1/num_echoes.
    
       ----------------------------------------------------------------------
    
       required parameters:
    
          -echo_times "TE1 TE2 ..." - specify echo times
                                      (use quotes to pass list as one parameter)
    
               e.g. -echo_times "15 30.5 41"
    
            Specify echo times as a list.
    
            Use either -echo_times or -echo_times_files.
    
    
          -echo_times_file FILE     - specify file with echo times
                                      (e.g. it could contain 15 30.5 41)
    
            Specify echo times from a text file.
    
            Use either -echo_times or -echo_times_files.
    
    
          -echo_dsets D1 D2 D3      - specify one run of multi-echo EPI data, e.g.:
    
               e.g. -echo_dsets pb03.SUBJ.r01.e*.volreg+tlrc.HEAD
    
            Provide the echo datasets for a single run of multi-echo EPI data.
    
    
       general options:
    
          -def_to_equal yes/no      - specify whether to default to equal weights
                                      (default = no)
    
            In the case where T2* seems huge or <= 0, or if the sum of the
            fractional weights is not close to 1 (see -tolerance), one might
            want to apply default weights equal to 1/num_echoes (so echoes
            are weighted equally).
    
            Without this, the weighting for such 'bad' voxels is based on the
            T2* limit.  See -t2_star_limit.
    
          -oc_method METHOD         - specify which method to employ
    
               e.g.     -oc_method OC_B
               default: -oc_method OC_A
    
            The OC_B method differs from OC_A by solving for T2* using log(mean())
            to solving log() over time, with the intention of being more accurate.
    
               methods:
    
                  OC_A      : compute T2* from log(mean(time series))
                              this is the original implementation
    
                  OC_B      : compute T2* from log(time series)
    
             * So far, testing has shown almost undetectable differences, so it
               may be a moot point.
    
          -prefix PREFIX            - specify prefix of resulting OC weights dataset
    
               e.g. -prefix OC.weights.SUBJ
    
          -sum_weight_tolerance TOL - tolerance for summed weight diff from 1.0
                                      (default = 0.001)
    
               e.g. -sum_weight_tolerance 0.0001
    
            This option only applies to the "-def_to_equal yes" case.
    
            If echo means (at some voxel) do not follow a decay curve, there
            could be truncation errors in weighting computation that lead to
            weights which do not sum to 1.0.  If abs(1-sum) > tolerance, such a
            voxel will be set in the tolerance.fail dataset.
    
            The default effect of this failure is to get equal weights across
            the echoes.
    
          -t2_star_limit LIMIT      - specify limit for T2* values
                                      (default = 300)
    
            When the system of equations does not show a reasonably fast decay,
            the slopes will be such that T2* is huge or possibly negative.  In such
            cases, it is applied as the LIMIT from this option.
    
          -work_dir WDIR            - specify directory to compute results in
    
            All the processing is done in a new sub-directory.  If this program
            is to be applied one run at a time, it is important to specify such
            working directories to keep the names unique.
    
          -verb                     - increase verbosity of output
    
    
       terminal options:
    
           -help
           -hist
           -ver
    
       ----------------------------------------------------------------------
       R Reynolds, Feb, 2016               Thanks to Javier Gonzalez-Castillo
