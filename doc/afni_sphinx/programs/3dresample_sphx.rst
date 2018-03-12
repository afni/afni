**********
3dresample
**********

.. _3dresample:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    3dresample - reorient and/or resample a dataset
    
        This program can be used to change the orientation of a
        dataset (via the -orient option), or the dx,dy,dz
        grid spacing (via the -dxyz option), or change them
        both to match that of a master dataset (via the -master
        option).
    
        Note: if both -master and -dxyz are used, the dxyz values
              will override those from the master dataset.
    
     ** It is important to note that once a dataset of a certain
        grid is created (i.e. orientation, dxyz, field of view),
        if other datasets are going to be resampled to match that
        first one, then using -master should be used, instead of
        -dxyz.  That will guarantee that all grids match.
    
        Otherwise, even using both -orient and -dxyz, one may not
        be sure that the fields of view will identical, for example.
    
     ** Warning: this program is not meant to transform datasets
                 between view types (such as '+orig' and '+tlrc').
    
                 For that purpose, please see '3dfractionize -help'
                 or 'adwarp -help'.
    
    ------------------------------------------------------------
    
      usage: 3dresample [options] -prefix OUT_DSET -input IN_DSET
    
      examples:
    
        3dresample -orient asl -rmode NN -prefix asl.dset -input in+orig
        3dresample -dxyz 1.0 1.0 0.9 -prefix 119.dset -input in+tlrc
        3dresample -master master+orig -prefix new.dset -input old+orig
    
      note:
    
        Information about a dataset's voxel size and orientation
        can be found in the output of program 3dinfo
    
    ------------------------------------------------------------
    
      options: 
    
        -help            : show this help information
    
        -hist            : output the history of program changes
    
        -debug LEVEL     : print debug info along the way
              e.g.  -debug 1
              default level is 0, max is 2
    
        -version         : show version information
    
        -bound_type TYPE : specify which boundary is preserved
              e.g.  -bound_type SLAB
              default is FOV (field of view)
    
              The default and original use preserves the field of
              of view when resampling, allowing the extents (SLABs)
              to grow or shrink by half of the difference in the
              dimension size (big voxels to small will cause the
              extents to expand, for example, while small to big
              will cause them to shrink).
    
              Using -bound_type SLAB will have the opposite effect.
              The extents should be unchanged, while the FOV will
              grow or shrink in the opposite way as above).
    
              Note that when using SLAB, edge voxels should be
              mostly unaffected by the interpolation.
    
        -dxyz DX DY DZ   : resample to new dx, dy and dz
              e.g.  -dxyz 1.0 1.0 0.9
              default is to leave unchanged
    
              Each of DX,DY,DZ must be a positive real number,
              and will be used for a voxel delta in the new
              dataset (according to any new orientation).
    
        -orient OR_CODE  : reorient to new axis order.
              e.g.  -orient asl
              default is to leave unchanged
    
              The orientation code is a 3 character string,
              where the characters come from the respective
              sets {A,P}, {I,S}, {L,R}.
    
              For example OR_CODE = LPI is the standard
              'neuroscience' orientation, where the x-axis is
              Left-to-Right, the y-axis is Posterior-to-Anterior,
              and the z-axis is Inferior-to-Superior.
    
        -rmode RESAM     : use this resampling method
              e.g.  -rmode Linear
              default is NN (nearest neighbor)
    
              The resampling method string RESAM should come
              from the set {'NN', 'Li', 'Cu', 'Bk'}.  These
              are for 'Nearest Neighbor', 'Linear', 'Cubic'
              and 'Blocky' interpolation, respectively.
    
              For details, go to the 'Define Datamode' panel
              of the afni GUI, click BHelp and then the
              'ULay resam mode' menu.
    
        -master MAST_DSET: align dataset grid to that of MAST_DSET
              e.g.  -master master.dset+orig
    
              Get dxyz and orient from a master dataset.  The
              resulting grid will match that of the master.  This
              option can be used with -dxyz, but not with -orient.
    
        -prefix OUT_DSET : required prefix for output dataset
              e.g.  -prefix reori.asl.pickle
    
        -input IN_DSET   : required input dataset to reorient
              e.g.  -input old.dset+orig
    
        -inset IN_DSET   : alternative to -input
    ------------------------------------------------------------
    
      Author: R. Reynolds - Version 1.10 <June 26, 2014>
