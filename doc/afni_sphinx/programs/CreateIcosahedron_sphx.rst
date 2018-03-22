*****************
CreateIcosahedron
*****************

.. _ahelp_CreateIcosahedron:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: CreateIcosahedron [-rad r] [-rd recDepth] [-ld linDepth] 
                             [-ctr ctr] [-prefix fout] [-help]
    
       -rad r: size of icosahedron. (optional, default 100)
            The edge length l = 4 r / sqrt(10+2sqrt(5)) 
            The area a = 5 sqrt(3) l^2 
            The volume v = 5/12 (3+sqrt(5)) l^3 
    
       -rd recDepth: recursive (binary) tesselation depth for icosahedron 
           (optional, default:3) 
           (recommended to approximate number of nodes in brain: 6
           let rd2 = 2 * recDepth
           Nvert = 2 + 10 * 2^rd2
           Ntri  = 20 * 2^rd2
           Nedge = 30 * 2^rd2
    
       -ld linDepth: number of edge divides for linear icosahedron tesselation
           (optional, default uses binary tesselation).
           Nvert = 2 + 10 * linDepth^2
           Ntri  = 20 * linDepth^2
           Nedge = 30 * linDepth^2
    
       -min_nodes MIN_NODES: Automatically select the -ld value which produces an
                             icosahedron of at least MIN_NODES nodes.
    
       -nums: output the number of nodes (vertices), triangles, edges, 
              total volume and total area then quit
    
       -nums_quiet: same as -nums but less verbose. For the machine in you.
    
       -ctr ctr: coordinates of center of icosahedron. 
           (optional, default 0,0,0)
    
       -tosphere: project nodes to sphere.
    
       -prefix fout: prefix for output files. 
           (optional, default CreateIco)
                     The surface is written out in FreeSurfer's .asc
                     format by default. To change that, include a
                     valid extension to the prefix such as: fout.gii 
    
       -help: help message
    
    
    Compile Date:
       Mar 22 2018
    
    
           Brenna D. Argall LBC/NIMH/NIH bargall@codon.nih.gov 
