.. _ahelp_@fat_tract_colorize:

*******************
@fat_tract_colorize
*******************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
        The purpose of this function is to help visualize tractographic
        output of 3dTrackID, in particular the fully probabilistic mode
        ('-mode PROB') that doesn't output a set of 1D tracts for
        viewing. Here, local orientation of WM is viewed on a surface that
        encloses the tracked region, and the brightness is modulated by
        the FA.
    
        There are two halves to this program, each of which can be run
        separately.  First, V1 and FA values are used to compute RGB ->
        HSL coloration. An smoothed isosurface surrounding the tracked
        region is computed, and the coloration is mapped onto the surface.
        Second, both AFNI and SUMA are opened up with 'talking' turned on,
        and the data sets are visualized: the thresholded FA>0.2 map in
        AFNI, and the RGB colorized surface in SUMA.
    
    -------------------------------------------------------------------------
    
      OUTPUTS:
    
        1) PREFIX_RGB_HUE.nii.gz :an HSL coloration volume file with four
                                  bricks from the V1 and FA volumes:
                                    [0] Hue
                                    [1] Saturation
                                    [2] Luminosity
                                    [3] Brightness
        2) PREFIX_RGB_iso.ply    :a slightly smoothed isosurface file made by
                                  IsoSurface
        3) PREFIX_RGB_iso.spec   :a spec file made by quickspec.  Useful 
                                  description, huh?
        4) PREFIX_RGB.niml.dset  :a projection of appropriate coloration onto
                                  the surface
    
        ... and a set of AFNI+SUMA commands will also open up viewers and
            drive them with appropriate over/underlays and some
            probably-useful parameter settings.
    
    -------------------------------------------------------------------------
    
      RUNNING:
        @fat_tract_colorize -in_fa FILE_FA  -in_v1 FILE_V1             \
                            -in_tracts FILE_TR  -prefix PREFIX         \
                            { -in_ulay FILE_UL }                       \
                            { -no_view }  { -only_view }
        where:
        
        -in_fa FILE_FA     :FA values of the DT fitting, which can be used to
                            modulate the brightness of the RGB coloration.
        -in_v1 FILE_V1     :first eigenvector of the DT fitting, such as by
                            3dDWItoDT. The volume is supposed to be a unit 
                            vector with 3 components. The magnitudes of the 
                            components are each between [0, 1], so that
                            (|x|, |y|, |z|) -> gets mapped to (R, G, B).
        -in_tracts FILE_TR :the INDIMAP or PAIRMAP file output by 3dTrackID, 
                            specifying the subbrick as well, if there are >1
                            in it (you likely need to put the subbrick in 
                            quotes, like NAME_INDIMAP+orig'[0]').
        -prefix PREFIX     :prefix of all output files.
    
        -in_ulay FILE_UL   :optional ability load in a separate data set to
                            underlay in both the AFNI SUMA viewers (as
                            '-vol ...'  slices in SUMA).  For example, you
                            might want to to load in an anatomical
                            volume. Default is to use the FA data set.
    
        -no_view           :switch to turn off the auto-running of AFNI_SUMA
                            commands to view the output immediately
        -only_view         :switch to *only* view the data with AFNI+SUMA.
                            This assumes that you have run the command at least
                            once previously, so that there be data to view.
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
        
        @fat_tract_colorize -in_fa DTI/DT_FA+orig.                      \
                            -in_v1 DTI/DT_V1+orig.                      \
                            -in_tracts DTI/o.NET_000_PAIRMAP+orig'[0]'  \
                            -prefix RGB
    
    -------------------------------------------------------------------------
