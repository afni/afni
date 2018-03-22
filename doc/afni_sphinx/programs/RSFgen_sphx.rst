******
RSFgen
******

.. _ahelp_RSFgen:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Sample program to generate random stimulus functions.                  
                                                                           
    Usage:                                                                 
    RSFgen                                                          
    -nt n            n = length of time series                             
    -num_stimts p    p = number of input stimuli (experimental conditions) 
    [-nblock i k]    k = block length for stimulus i  (1<=i<=p)            
                         (default: k = 1)                                  
    [-seed s]        s = random number seed                                
    [-quiet]         flag to suppress screen output                        
    [-one_file]      place stimulus functions into a single .1D file       
    [-one_col]       write stimulus functions as a single column of decimal
                       integers (default: multiple columns of binary nos.) 
    [-prefix pname]  pname = prefix for p output .1D stimulus functions    
                       e.g., pname1.1D, pname2.1D, ..., pnamep.1D          
                                                                           
    The following Random Permutation, Markov Chain, and Input Table options
    are mutually exclusive.                                                
                                                                           
    Random Permutation options:                                            
    -nreps i r       r = number of repetitions for stimulus i  (1<=i<=p)   
    [-pseed s]       s = stim label permutation random number seed         
                                         p                                 
                     Note: Require n >= Sum (r[i] * k[i])                  
                                        i=1                                
                                                                           
    Markov Chain options:                                                  
    -markov mfile    mfile = file containing the transition prob. matrix   
    [-pzero z]       probability of a zero (i.e., null) state              
                         (default: z = 0)                                  
                                                                           
    Input Table row permutation options:                                   
    [-table dfile]   dfile = filename of column or table of numbers        
                     Note: dfile may have a column selector attached       
                     Note: With this option, all other input options,      
                           except -seed and -prefix, are ignored           
                                                                           
                                                                           
    Warning: This program will overwrite pre-existing .1D files            
