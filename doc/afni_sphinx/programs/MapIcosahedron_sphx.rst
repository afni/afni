**************
MapIcosahedron
**************

.. _MapIcosahedron:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: MapIcosahedron <-spec specFile> 
                          [-rd recDepth] [-ld linDepth] 
                          [-morph morphSurf] 
                          [-it numIt] [-prefix fout] 
                          [-NN_dset_map DSET]
                          [-dset_map DSET] [-fix_cut_surfaces]
                          [-verb] [-help] [...]
    
    Creates new versions of the original-mesh surfaces using the mesh
    of an icosahedron. 
    
       -spec specFile: spec file containing original-mesh surfaces
            including the spherical and warped spherical surfaces.
    
            You cannot use a specfile that has surfaces from both hemispheres.
            Such spec files are usually called *_both.spec
            You will need to run MapIcosahedron separately on each hemisphere
            then use the program inspec to merge the two new spec files.
            Say MapIcosahedron produced std.LH.spec and std.RH.spec, 
            you can use the following to merge them:
                  inspec   -LRmerge std.LH_SPEC.spec std.RH_SPEC.spec \
                           -prefix std.BOTH_SPEC.spec
    
       -rd recDepth: recursive (binary) tesselation depth for icosahedron.
            (optional, default:3) See CreateIcosahedron for more info.
    
       -ld linDepth: number of edge divides for linear icosahedron tesselation 
            (optional, default uses binary tesselation).
            See CreateIcosahedron -help for more info.
    
       *Note: Enter -1 for recDepth or linDepth to let program 
              choose a depth that best approximates the number of nodes in
              original-mesh surfaces.
    
       -morph morphSurf: 
    
       -NN_dset_map DSET: Map DSET onto the new mesh. 
                          Use Nearest Neighbor interpolation
       -dset_map DSET:  Same as NN_dset_map but with barycentric interpolation
    
       *Note: You can use repeated instances of either -NN_dset_map or -dset_map
              to process multiple datasets at once.
    
            Old Usage:
            ----------
            State name of spherical surface to which icosahedron 
            is inflated. Typical example for FreeSurfer surfaces would be 
            'sphere.reg', and that's the default used by the program. 
    
            New Usage:
            ----------
            State name or filename of spherical surface to which icosahedron 
            is inflated. Typical example for FreeSurfer surfaces would be 
            'sphere.reg', and that's the default used by the program. 
            Searching is first done assuming a State name and if that does
            not return exactly one match, a search based on the filename
            is carried out.
    
       The following four options affect the geometric center and radius
       settings of morphSurf. In previous versions, the geometric center
       was set to the center of mass. A better estimate of the geometric
       center is now obtained and this might make standard-mesh surfaces
       less sensitive to distortions in the spherical surfaces.
       With this change, the coordinates of the nodes will be silghtly
       different from in previous versions. If you insist on the old 
       method, use the option -use_com below.
       ----------------------------------------------------------------
       -sphere_at_origin: Geometric center of morphSurf sphere is at 
                          0.0 0.0 0.0. This is usually the case but
                          if you do not know, let the program guess.
    
       -sphere_center cx cy cz: Geometric center of morphSurf sphere. 
                                If not specified, it will be estimated.
          Note: It is best to specify cx cy cz or use -sphere_at_origin
                when the center is known.
    
       -use_com: (ONLY for backward compatibility)
                 Use this option to make the center of mass of morpSurf.
                 be the geometric center estimate. This is not optimal,
                 use this option only for backward compatibility.
                 The new results, i.e. without -use_com, should always be
                 better.
    
       -sphere_radius R: Radius of morphSurf sphere. If not specified,
                         this would be the average radius of morpSurf.
                         
       ----------------------------------------------------------------
    
       -it numIt: number of smoothing interations 
            (optional, default none).
    
       -prefix FOUT: prefix for output files.
            (optional, default 'std.')
    
       -morph_sphere_check: Do some quality checks on morphSurf and exit.
                            This option now replaces -sph_check and -sphreg_check
                            See output of SurfQual -help for more info on this
                            option's output.
    
    **********************************************
    -sph_check and -sphreg_check are now OBSOLETE. 
    
       [-sph_check]:(OBSOLETE, use -morph_sphere_check instead) 
                    Run tests for checking the spherical surface (sphere.asc)
                    The program exits after the checks.
                    This option is for debugging FreeSurfer surfaces only.
    
       [-sphreg_check]: (OBSOLETE, use -morph_sphere_check instead)
                    Run tests for checking the spherical surface (sphere.reg.asc)
                    The program exits after the checks.
                    This option is for debugging FreeSurfer surfaces only.
    
       -sph_check and -sphreg_check are mutually exclusive.
    
    **********************************************
    
       -all_surfs_spec: When specified, includes original-mesh surfaces 
           and icosahedron in output spec file.
           (optional, default does not include original-mesh surfaces)
       -verb: verbose.
       -fix_cut_surfaces: Check and fix standard-mesh surfaces with cuts for 
                          cross-cut connections.
       -check_cut_surfaces: (default) Check standard-mesh surfaces with cuts for 
                          cross-cut connections.
       -forget_cut_surface: Do not check standard-mesh surfaces with cuts for 
                          cross-cut connections.
       -write_nodemap: (default) Write a file showing the mapping of each 
                       node in the icosahedron to the closest
                       three nodes in the original mesh.
                       The file is named by the prefix of the output
                       spec file and suffixed by MI.1D
      NOTE I: This option is useful for understanding what contributed
            to a node's position in the standard meshes (STD_M).
            Say a triangle on the  STD_M version of the white matter
            surface (STD_WM) looks fishy, such as being large and 
            obtuse compared to other triangles in STD_M. Right
            click on that triangle and get one of its nodes (Ns)
            search for Ns in column 0 of the MI.1D file. The three
            integers (N0, N1, N2) on the same row as Ns will point 
            to the three nodes on the original meshes (sphere.reg) 
            to which Ns (from the icosahedron) was mapped. Go to N1
            (or N0 or N2) on the original sphere.reg and examine the
            mesh there, which is best seen in mesh view mode ('p' button).
            It will most likely be the case that the sphere.reg mesh
            there would be highly distorted (quite compressed).
      NOTE II: The program also outputs a new mapping file in the format
            that SurfToSurf  likes. This format has the extension .niml.M2M
            This way you can use SurfToSurf to map a new dataset from original
            to standard meshes in the same way that MapIcosahedron would have
            carried out the mapping.
            For example, the following command creates standard meshes and
            also maps thickness data onto the new meshes:
                MapIcosahedron -spec rh.spec -ld 60 \
                               -dset_map rh.thickness.gii.dset \
                               -prefix std.60.
            Say you want to map another (SOMEDSET) dataset defined on the
            orignal mesh onto the std.60 mesh and use the same mapping derived 
            by MapIcosahedron. The command for that would be:
                SurfToSurf -i_fs std.60.rh.smoothwm.asc \
                           -i_fs rh.smoothwm.asc \
                           -prefix std.60. \
                           -mapfile std.60.rh.niml.M2M \
                           -dset rh.SOMEDSET.gii.dset
       -no_nodemap: Opposite of write_nodemap
       -write_dist PREFIX: write distortions to PREFIX.LABEL
            The mapping for 0,0,0-centered surfaces was previously distorted.
            Write a file containing the node-wise distortion vectors.
            One could then summarize that file using 1d_tool.py, as in:
                1d_tool.py -collapse_cols euclidean_norm -show_mmms \
                           -infile PREFIX.LABEL.txt
            or simply write out the euclidean norms for suma display:
                1d_tool.py -collapse_cols euclidean_norm \
                           -infile PREFIX.LABEL.txt -write PREFIX.enorm.1D
    
    NOTE 1: The algorithm used by this program is applicable
          to any surfaces warped to a spherical coordinate
          system. However for the moment, the interface for
          this algorithm only deals with FreeSurfer surfaces.
          This is only due to user demand and available test
          data. If you want to apply this algorithm using surfaces
          created by other programs such as SureFit and Caret, 
          Send saadz@mail.nih.gov a note and some test data.
    
    NOTE 2: At times, the standard-mesh surfaces are visibly
          distorted in some locations from the original surfaces.
          So far, this has only occurred when original spherical 
          surfaces had topological errors in them. 
          See SurfQual -help and SUMA's online documentation 
          for more detail.
    
    
    Compile Date:
       Mar  7 2018
    
    
              Brenna D. Argall LBC/NIMH/NIH  
    (contact) Ziad S. Saad     SSC/NIMH/NIH saadz@mail.nih.gov
    
