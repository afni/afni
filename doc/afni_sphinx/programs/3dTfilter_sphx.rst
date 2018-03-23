.. _ahelp_3dTfilter:

*********
3dTfilter
*********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    3dTfilter takes as input a dataset, filters the time series in
    each voxel as ordered by the user, and outputs a new dataset.
    The data in each voxel is processed separately.
    
    The user (you?) specifies the filter functions to apply.
    They are applied in the order given on the command line:
      -filter rank -filter adaptive:7
    means to do the following operations
      (1) turn the data into ranks
      (2) apply the adaptive mean filter to the ranks
    
    Notes:
    ------
    ** This program is a work in progress, and more capabilities
       will be added as time allows, as the need arises, and as
       the author's whims bubble to the surface of his febrile brain.
    
    ** This program is for people who have Sisu.
    
    Options:
    --------
    
     -input inputdataset
    
     -prefix outputdataset
    
     -filter FunctionName
         At least one '-filter' option is required!
         The FunctionName values that you can give are:
    
            rank       = smallest value is replaced by 0,
                         next smallest value by 1, and so forth.
                         ** This filter is pretty useless.
    
            adaptive:H = adaptive mean filter with half-width of
                         'H' time points (H > 0).
                         ** At most one 'adaptive' filter can be used!
                         ** The filter 'footprint' is 2*H+1 points.
                         ** This filter does local smoothing over the
                            'footprint', with values far away from
                            the local median being weighted less.
    
            detrend:P  = (least squares) detrend with polynomials of up
                         order 'P' for P=0, 1, 2, ....
                         ** At most one 'detrend' filter can be used!
    
            despike    = apply the 'NEW25' despiking algorithm, as in
                         program 3dDespike.
    
    Example:
    --------
     3dTfilter -input fred.nii -prefix fred.af.nii -filter adaptive:7
    
    -------
    Author: The Programmer with No Name
    -------
