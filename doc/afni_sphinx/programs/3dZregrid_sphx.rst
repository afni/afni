*********
3dZregrid
*********

.. _ahelp_3dZregrid:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dZregrid [option] dataset
    Alters the input dataset's slice thickness and/or number.
    
    ***  For most purposes, this program has been superseded ***
    ***  by program 3dresample, which can change the grid of ***
    ***  a dataset in all 3 directions at once.              ***
    
    OPTIONS:
     -dz D     = sets slice thickness to D mm
     -nz N     = sets slice count to N
     -zsize Z  = sets thickness of dataset (center-to-center of
                  first and last slices) to Z mm
     -prefix P = write result in dataset with prefix P
     -verb     = write progress reports to stderr
    
    At least one of '-dz', '-nz', or '-zsize' must be given.
    On the other hand, using all 3 is over-specification.
    The following combinations make sense:
     -dz only                   ==> N stays fixed from input dataset
                                     and then is like setting Z = N*D
     -dz and -nz together       ==> like setting Z = N*D
     -dz and -zsize together    ==> like setting N = Z/D
     -nz only                   ==> D stays fixed from input dataset
                                     and then is like setting Z = N*D
     -zsize only                ==> D stays fixed from input dataset
                                     and then is like setting N = Z/D
     -nsize and -zsize together ==> like setting D = Z/N
    
    NOTES:
     * If the input is a 3D+time dataset with slice-dependent time
        offsets, the output will have its time offsets cleared.
        It probably makes sense to do 3dTshift BEFORE using this
        program in such a case.
     * The output of this program is centered around the same
        location as the input dataset.  Slices outside the
        original volume (e.g., when Z is increased) will be
        zero.  This is NOT the same as using 3dZeropad, which
        only adds zeros, and does not interpolate to a new grid.
     * Linear interpolation is used between slices.  However,
        new slice positions outside the old volume but within
        0.5 old slice thicknesses will get a copy of the last slice.
        New slices outside this buffer zone will be all zeros.
    
    EXAMPLE:
     You have two 3D anatomical datasets from the same subject that
     need to be registered.  Unfortunately, the first one has slice
     thickness 1.2 mm and the second 1.3 mm.  Assuming they have
     the same number of slices, then do something like
      3dZregrid -dz 1.2 -prefix ElvisZZ Elvis2+orig
      3dvolreg -base Elvis1+orig -prefix Elvis2reg ElvisZZ+orig
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
