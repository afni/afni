**********
GLTsymtest
**********

.. _GLTsymtest:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    The function of program GLTsymtest is to test a set of '-gltsym'
    strings -- for use with 3dDeconvolve or 3dREMLfit -- for validity.
    
    Usage:  GLTsymtest [options] varlist expr [expr ...]
    
       options (only 1 so far):
    
          -badonly : output only BAD messages, rather than all
    
    * 'varlist' is a list of allowed variable names in the expression.
      These names can be separated by commans, semicolons, and/or
      spaces (varlist would have to be in quotes if it contains spaces).
    
    * Each 'expr' is a GLT symbolic expression, which should be in quotes
      since different components are separated by blanks.
    
    EXAMPLES
    -------
      GLTsymtest -badonly 'Vrel Arel' 'Vrel -Arel' 'Verl + +aud'
    
      GLTsymtest 'Vrel Arel' 'Vrel -Arel' 'Verl + +aud'
    
      The first expression is good, but the second has both variable names
      mis-typed; the output from this program would include these messages:
    
        ***** Scanned GLT messages *****
        ++ -gltsym is: 'Vrel -Arel'
        ++ INFO: Allowed variable list is 'Vrel Arel'
        ++ INFO: This gltsym appears to be OKAY :-)
    
        ***** Scanned GLT messages *****
        ++ -gltsym is: 'Verl + +aud'
        ++ INFO: Allowed variable list is 'Vrel Arel'
        ++ INFO: -gltsym: isolated '+' is being ignored
        ** ERROR: -gltsym: can't match symbolic name 'Verl'
        ** ERROR: -gltsym: can't match symbolic name 'aud'
        ** SORRY: This gltsym appears to be BAD :-(
    
    NOTES
    -----
    * GLTsymtest does not check subscripts on variable names against the legal
      range for the name, since the information about the dimensionality of
      the beta vector associated with each name is not available here.
    
    * The exit status for this program is the number of expressions that had
      at least one ERROR message.  In the example above, this status would be 1.
    
    * The text output goes to stdout.
    
    
    * Authored by RWCox on May Day 2015 to aid Rick Reynolds in detecting such
      problems, induced for example when his boss does someting stupid during
      an AFNI bootcamp in South Africa (a purely hypothetical case, I assure you).
