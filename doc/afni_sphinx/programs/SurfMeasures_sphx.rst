************
SurfMeasures
************

.. _SurfMeasures:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    SurfMeasures - compute measures from the surface dataset(s)
    
      usage: SurfMeasures [options] -spec SPEC_FILE -out OUTFILE
    
        This program is meant to read in a surface or surface pair,
        and to output and user-requested measures over the surfaces.
        The surfaces must be specified in the SPEC_FILE.
    
     ** Use the 'inspec' command for getting information about the
        surfaces in a spec file.
    
        The output will be a 1D format text file, with one column
        (or possibly 3) per user-specified measure function.  Some
        functions require only 1 surface, some require 2.
    
        Current functions (applied with '-func') include:
    
            ang_norms    : angular difference between normals
            ang_ns_A     : angular diff between segment and first norm
            ang_ns_B     : angular diff between segment and second norm
            coord_A      : xyz coordinates of node on first surface
            coord_B      : xyz coordinates of node on second surface
            n_area_A     : associated node area on first surface
            n_area_B     : associated node area on second surface
            n_avearea_A  : for each node, average area of triangles (surf A)
            n_avearea_B  : for each node, average area of triangles (surf B)
            n_ntri       : for each node, number of associated triangles
            node_vol     : associated node volume between surfaces
            node_volg    : associated node volume between surfaces via Gauss' theorem
            nodes        : node number
            norm_A       : vector of normal at node on first surface
            norm_B       : vector of normal at node on second surface
            thick        : distance between surfaces along segment
    
    ------------------------------------------------------------
    
      examples:
    
        1. For each node on the surface smoothwm in the spec file,
           fred.spec, output the node number (the default action),
           the xyz coordinates, and the area associated with the
           node (1/3 of the total area of triangles having that node
           as a vertex).
    
            SurfMeasures                                   \
                -spec       fred1.spec                     \
                -sv         fred_anat+orig                 \
                -surf_A     smoothwm                       \
                -func       coord_A                        \
                -func       n_area_A                       \
                -out_1D     fred1_areas.1D                   
    
        2. For each node of the surface pair smoothwm and pial,
           display the:
             o  node index
             o  node's area from the first surface
             o  node's area from the second surface
             o  node's resulting volume
             o  thickness at that node (segment distance)
             o  coordinates of the first segment node
             o  coordinates of the second segment node
    
             Additionally, display total surface areas, minimum and
             maximum thicknesses, and total volume for the
             cortical ribbon (the sum of node volumes).
    
            SurfMeasures                                   \
                -spec       fred2.spec                     \
                -sv         fred_anat+orig                 \
                -surf_A     smoothwm                       \
                -surf_B     pial                           \
                -func       n_area_A                       \
                -func       n_area_B                       \
                -func       node_volg                      \
                -func       thick                          \
                -func       coord_A                        \
                -func       coord_B                        \
                -info_area                                 \
                -info_thick                                \
                -info_vol                                  \
                -out        fred2_vol.niml.dset              
    
        3. For each node of the surface pair, display the:
             o  node index
             o  angular diff between the first and second norms
             o  angular diff between the segment and first norm
             o  angular diff between the segment and second norm
             o  the normal vectors for the first surface nodes
             o  the normal vectors for the second surface nodes
             o  angular diff between the segment and second norm
    
            SurfMeasures                                   \
                -spec       fred2.spec                     \
                -surf_A     smoothwm                       \
                -surf_B     pial                           \
                -func       ang_norms                      \
                -func       ang_ns_A                       \
                -func       ang_ns_B                       \
                -func       norm_A                         \
                -func       norm_B                         \
                -out        fred2_norm_angles                
    
        4. Similar to #3, but output extra debug info, and in
           particular, info regarding node 5000.
    
            SurfMeasures                                   \
                -spec       fred2.spec                     \
                -sv         fred_anat+orig                 \
                -surf_A     smoothwm                       \
                -surf_B     pial                           \
                -func       ang_norms                      \
                -func       ang_ns_A                       \
                -func       ang_ns_B                       \
                -debug      2                              \
                -dnode      5000                           \
                -out        fred2_norm_angles.1D             
    
        5. For each node, output the  volume, thickness
           and areas, but restrict the nodes to the list contained in
           column 0 of file sdata.1D.  Furthermore, restrict those 
           nodes to the mask inferred by the given '-cmask' option.
    
            SurfMeasures                                                   \
                -spec       fred2.spec                           \
                -sv         fred_anat+orig                       \
                -surf_A     smoothwm                             \
                -surf_B     pial                                 \
                -func       node_volg                            \
                -func       thick                                \
                -func       n_area_A                             \
                -func       n_area_B                             \
                -nodes_1D   'sdata.1D[0]'                        \
                -cmask      '-a sdata.1D[2] -expr step(a-1000)'  \
                -out        fred2_masked.1D                        
    
    ------------------------------------------------------------
    
      REQUIRED COMMAND ARGUMENTS:
    
        -spec SPEC_FILE       : SUMA spec file
    
            e.g. -spec fred2.spec
    
            The surface specification file contains a list of
            related surfaces.  In order for a surface to be
            processed by this program, it must exist in the spec
            file.
    
        -surf_A SURF_NAME     : surface name (in spec file)
        -surf_B SURF_NAME     : surface name (in spec file)
    
            e.g. -surf_A smoothwm
            e.g. -surf_A lh.smoothwm
            e.g. -surf_B lh.pial
    
            This is used to specify which surface(s) will be used
            by the program.  The 'A' and 'B' correspond to other
            program options (e.g. the 'A' in n_area_A).
    
            The '-surf_B' parameter is required only when the user
            wishes to input two surfaces.
    
            Any surface name provided must be unique in the spec
            file, and must match the name of the surface data file
            (e.g. lh.smoothwm.asc).
    
        -out_1D OUT_FILE.1D   : 1D output filename
    
            e.g. -out_1D pickle_norm_info.1D
    
            This option is used to specify the name of the output
            file.  The output file will be in the 1D ascii format,
            with 2 rows of comments for column headers, and 1 row
            for each node index.
    
            There will be 1 or 3 columns per '-func' option, with
            a default of 1 for "nodes".
    
            Consider using the newer -out instead of -out_1D
    
    
        -out OUT_DSET   : Output into surface dataset OUT_DSET
    
            e.g. -out pickle_norm_info.niml.dset
    
            The dset format is determined from the extension of
            OUT_DSET. Default is NIML format.
            You are better off using -out and non-1D format datasets
            because non-1D datasets are better handled by 3dcalc
    
            You can use both -out and -out_1D, but why would you do this?
    
    ------------------------------------------------------------
    
      ALPHABETICAL LISTING OF OPTIONS:
    
        -cmask COMMAND        : restrict nodes with a mask
    
            e.g.     -cmask '-a sdata.1D[2] -expr step(a-1000)'
    
            This option will produce a mask to be applied to the
            list of surface nodes.  The total mask size, including
            zero entries, must match the number of nodes.  If a
            specific node list is provided via the '-nodes_1D'
            option, then the mask size should match the length of
            the provided node list.
            
            Consider the provided example using the file sdata.1D.
            If a surface has 100000 nodes (and no '-nodes_1D' option
            is used), then there must be 100000 values in column 2
            of the file sdata.1D.
    
            Alternately, if the '-nodes_1D' option is used, giving
            a list of 42 nodes, then the mask length should also be
            42 (regardless of 0 entries).
    
            See '-nodes_1D' for more information.
    
        -debug LEVEL          : display extra run-time info
    
            e.g.     -debug 2
            default: -debug 0
    
            Valid debug levels are from 0 to 5.
    
        -dnode NODE           : display extra info for node NODE
    
            e.g. -dnode 5000
    
            This option can be used to display extra information
            about node NODE during surface evaluation.
    
        -func FUNCTION        : request output for FUNCTION
    
            e.g. -func thick
    
            This option is used to request output for the given
            FUNCTION (measure).  Some measures produce one column
            of output (e.g. thick or ang_norms), and some produce
            three (e.g. coord_A).  These options, in the order they
            are given, determine the structure of the output file.
    
            Current functions include:
    
                ang_norms    : angular difference between normals
                ang_ns_A     : angular diff between segment and first norm
                ang_ns_B     : angular diff between segment and second norm
                coord_A      : xyz coordinates of node on first surface
                coord_B      : xyz coordinates of node on second surface
                n_area_A     : associated node area on first surface
                n_area_B     : associated node area on second surface
                n_avearea_A  : for each node, average area of triangles (surf A)
                n_avearea_B  : for each node, average area of triangles (surf B)
                n_ntri       : for each node, number of associated triangles
                node_vol     : associated node volume between surfaces
                node_volg    : associated node volume between surfaces via Gauss' theorem
                nodes        : node number
                norm_A       : vector of normal at node on first surface
                norm_B       : vector of normal at node on second surface
                thick        : distance between surfaces along segment
    
              Note that with node_vol, the node volumes can be a little
              biased. It is recommended you use -node_volg instead.
    
              You can also use -func ALL to get everything output.
              You should not use other -func options with -func ALL
    
        -help                 : show this help menu
    
        -hist                 : display program revision history
    
            This option is used to provide a history of changes
            to the program, along with version numbers.
    
      NOTE: the following '-info_XXXX' options are used to display
            pieces of 'aggregate' information about the surface(s).
    
        -info_all             : display all final info
    
            This is a short-cut to get all '-info_XXXX' options.
    
        -info_area            : display info on surface area(s)
    
            Display the total area of each triangulated surface.
    
        -info_norms           : display info about the normals
    
            For 1 or 2 surfaces, this will give (if possible) the
            average angular difference between:
    
                o the normals of the surfaces
                o the connecting segment and the first normal
                o the connecting segment and the second normal
    
        -info_thick           : display min and max thickness
    
            For 2 surfaces, this is used to display the minimum and
            maximum distances between the surfaces, along each of
            the connecting segments.
    
        -info_vol             : display info about the volume
    
            For 2 surfaces, display the total computed volume.
            Note that this node-wise volume computation is an
            approximation, and tends to run ~10 % high.
    
            ** for more accuracy, use -info_volg **
    
        -info_volg             : display info about the volume
                                 which is estimated with Gauss'
                                 theorem.
    
        -nodes_1D NODELIST.1D : request output for only these nodes
    
            e.g.  -nodes_1D node_index_list.1D
            e.g.  -nodes_1D sdata.1D'[0]'
    
            The NODELIST file should contain a list of node indices.
            Output from the program would then be restricted to the
            nodes in the list.
            
            For instance, suppose that the file BA_04.1D contains
            a list of surface nodes that are located in Broadman's
            Area 4.  To get output from the nodes in that area, use:
            
                -nodes_1D BA_04.1D
            
            For another example, suppose that the file sdata.1D has
            node indices in column 0, and Broadman's Area indices in
            column 3.  To restrict output to the nodes in Broadman's
            area 4, use the pair of options:
            
                -nodes_1D 'sdata.1D[0]'                     \
                -cmask '-a sdata.1D[3] -expr (1-bool(a-4))' 
    
        -sv SURF_VOLUME       : specify an associated AFNI volume
    
            e.g. -sv fred_anat+orig
    
            If there is any need to know the orientation of the
            surface, a surface volume dataset may be provided.
    
        -ver                  : show version information
    
            Show version and compile date.
    
    ------------------------------------------------------------
    
      Author: R. Reynolds  - version 1.11 (October 6, 2004)
