.. _ahelp_@fix_FSsphere:

*************
@fix_FSsphere
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @fix_FSsphere <-spec SPEC> <-sphere SPHERE.asc>
                         [-niter NITER] [-lim LIM] [-keep_temp]
                         [-project_first]
    
       Fixes errors in FreeSurfer spherical surfaces.
       Mandatory parameters:
       -spec SPEC: Spec file
       -sphere SPHERE.asc: SPHERE.asc is the sphere to be used.
       Optional parameters:
       -niter NITER: Number of local smoothing operations.
                     Default is 3000
       -lim LIM: Extent, in mm, by which troubled sections 
                 are fattened. Default is 6
       -project_first: Project to a sphere, before smoothing.
                       Default is: 0
    
       Output:
       Corrected surface is called SPHERE_fxd.asc
    
    Example:
    @fix_FSsphere -spec ./2005-10-01-km_rh.spec -sphere ./rh.sphere.asc
