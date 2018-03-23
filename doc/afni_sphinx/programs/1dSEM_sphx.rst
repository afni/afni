.. _ahelp_1dSEM:

*****
1dSEM
*****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dSEM [options] -theta 1dfile -C 1dfile -psi 1dfile -DF nn.n
    Computes path coefficients for connection matrix in Structural Equation
        Modeling (SEM)
     The program takes as input :
        1. A 1D file with an initial representation of the connection matrix
           with a 1 for each interaction component to be modeled and a 0 if
           if it is not to be modeled. This matrix should be PxP rows and column
        2. A 1D file of the C, correlation matrix, also with dimensions PxP
        3. A 1D file of the residual variance vector, psi
        4. The degrees of freedom, DF
    
        Output is printed to the terminal and may be redirected to a 1D file
        The path coefficient matrix is printed for each matrix computed
     Options:
       -theta file.1D = connection matrix 1D file with initial representation
       -C file.1D = correlation matrix 1D file
       -psi file.1D = residual variance vector 1D file
       -DF nn.n = degrees of freedom
       -max_iter n = maximum number of iterations for convergence (Default=10000).
        Values can range from 1 to any positive integer less than 10000.
       -nrand n = number of random trials before optimization (Default = 100)
       -limits m.mmm n.nnn = lower and upper limits for connection coefficients
        (Default = -1.0 to 1.0)
       -calccost = no modeling at all, just calculate the cost function for the
        coefficients as given in the theta file. This may be useful for verifying
        published results
       -verbose nnnnn = print info every nnnnn steps
    
     Model search options:
     Look for best model. The initial connection matrix file must follow these
       specifications. Each entry must be 0 for entries excluded from the model,
       1 for each required entry in the minimum model, 2 for each possible path
       to try.
       -tree_growth or 
       -model_search = search for best model by growing a model for one additional
        coefficient from the previous model for n-1 coefficients. If the initial
        theta matrix has no required coefficients, the initial model will grow from
        the best model for a single coefficient
       -max_paths n = maximum number of paths to include (Default = 1000)
       -stop_cost n.nnn = stop searching for paths when cost function is below
        this value (Default = 0.1)
       -forest_growth or 
       -grow_all = search over all possible models by comparing models at
        incrementally increasing number of path coefficients. This
        algorithm searches all possible combinations; for the number of coeffs
        this method can be exceptionally slow, especially as the number of
        coefficients gets larger, for example at n>=9.
       -leafpicker = relevant only for forest growth searches. Expands the search
        optimization to look at multiple paths to avoid local minimum. This method
        is the default technique for tree growth and standard coefficient searches
     This program uses a Powell optimization algorithm to find the connection
       coefficients for any particular model.
    
     References:
       Powell, MJD, "The NEWUOA software for unconstrained optimization without
        derivatives", Technical report DAMTP 2004/NA08, Cambridge University
        Numerical Analysis Group -- http://www.damtp.cam.ac.uk/user/na/reports.html
    
       Bullmore, ET, Horwitz, B, Honey, GD, Brammer, MJ, Williams, SCR, Sharma, T,
        How Good is Good Enough in Path Analysis of fMRI Data?
        NeuroImage 11, 289-301 (2000)
    
       Stein, JL, et al., A validated network of effective amygdala connectivity,
        NeuroImage (2007), doi:10.1016/j.neuroimage.2007.03.022
    
     The initial representation in the theta file is non-zero for each element
       to be modeled. The 1D file can have leading columns for labels that will
       be used in the output. Label rows must be commented with the # symbol
     If using any of the model search options, the theta file should have a '1' for
       each required coefficient, '0' for each excluded coefficient, '2' for an
       optional coefficient. Excluded coefficients are not modeled. Required
       coefficients are included in every computed model.
    
     N.B. - Connection directionality in the path connection matrices is from 
       column to row of the output connection coefficient matrices.
    
       Be very careful when interpreting those path coefficients.
       First of all, they are not correlation coefficients. Suppose we have a
       network with a path connecting from region A to region B. The meaning
       of the coefficient theta (e.g., 0.81) is this: if region A increases by 
       one standard deviation from its mean, region B would be expected to increase
       by 0.81 its own standard deviations from its own mean while holding all other
       relevant regional connections constant. With a path coefficient of -0.16, 
       when region A increases by one standard deviation from its mean, region B 
       would be expected to decrease by 0.16 its own standard deviations from its
       own mean while holding all other relevant regional connections constant.
    
       So theoretically speaking the range of the path coefficients can be anything,
       but most of the time they range from -1 to 1. To save running time, the
       default values for -limits are set with -1 and 1, but if the result hits
       the boundary, increase them and re-run the analysis.
    
     Examples:
       To confirm a specific model:
        1dSEM -theta inittheta.1D -C SEMCorr.1D -psi SEMvar.1D -DF 30
       To search models by growing from the best single coefficient model
         up to 12 coefficients
        1dSEM -theta testthetas_ms.1D -C testcorr.1D -psi testpsi.1D \ 
        -limits -2 2 -nrand 100 -DF 30 -model_search -max_paths 12
       To search all possible models up to 8 coefficients:
        1dSEM -theta testthetas_ms.1D -C testcorr.1D -psi testpsi.1D \ 
        -nrand 10 -DF 30 -stop_cost 0.1 -grow_all -max_paths 8 | & tee testgrow.txt
    
       For more information, see https://afni.nimh.nih.gov/sscc/gangc/PathAna.html
        and our HBM 2007 poster at
       https://afni.nimh.nih.gov/sscc/posters/file.2007-06-07.0771819246
     If you find this program useful, please cite:
       G Chen, DR Glen, JL Stein, AS Meyer-Lindenberg, ZS Saad, RW Cox,
       Model Validation and Automated Search in FMRI Path Analysis:
       A Fast Open-Source Tool for Structural Equation Modeling,
       Human Brain Mapping Conference, 2007
