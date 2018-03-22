*********
SurfClust
*********

.. _ahelp_SurfClust:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: A program to perform clustering analysis surfaces.
      SurfClust <[-spec SpecFile -surf_A insurf] [-i insurf]> 
                <-input inData.dset dcol_index> 
                <-rmm rad>
                [-amm2 minarea]
                [-n minnodes]
                [-prefix OUTPREF]  
                [-out_clusterdset] [-out_roidset] 
                [-out_fulllist]
                [-sort_none | -sort_n_nodes | -sort_area]
    
      The program can outputs a table of the clusters on the surface,
      a mask dataset formed by the different clusters and a clustered
      version of the input dataset.
    
      Mandatory parameters:
        Surface Input can be done with:
         -spec SpecFile: The surface spec file.
         -surf_A insurf: The input surface name.
        or with:
         -i insurf: With insurf being the full name of the surface.
    
         -input inData.dset dcol_index: The input dataset
                                      and the index of the
                                      datacolumn to use
                                      (index 0 for 1st column).
                                      Values of 0 indicate 
                                      inactive nodes.
         -rmm rad: Maximum distance between an activated node
                   and the cluster to which it belongs.
                   Distance is measured on the surface's graph (mesh).
                   If you want the distance to be in number of edges,
                   set rad to -N for an N edge max distance.
                   For example -rmm -2 means that nodes connected
                   by 1 or two edges are in a cluster.
    
      Optional Parameters:
         -thresh_col tcolind: Index of thresholding column.
                              Default is column 0.
          -thresh tval: Apply thresholding prior to clustering.
                       A node n is considered if thresh_col[n] >= tval.
         -athresh tval: Apply absolute thresholding prior to clustering.
                        A node n is considered if | thresh_col[n] | >= tval.
         -ir_range R0 R1: Apply thresholding in range.
                        A node n is considered if 
                           thresh_col[n] >= R0 && thresh_col[n] <= R1
         -ex_range R0 R1: Apply thresholding outside of range.
                        A node n is considered if 
                           thresh_col[n] < R0 || thresh_col[n] > R1
         -amm2 minarea: Do not output results for clusters having
                        an area less than minarea. 
                        If minarea < 0 AND -n is not set (or < 0)
                        then minnodes = -minarea . See option -n below.
         -n  minnodes: Do not output results for clusters having
                        less nodes than minnodes.
                        minnodes can get set with negative minarea above.
         -prefix OUTPREF: Prefix for output.
                          Default is the prefix of 
                          the input dataset.
                          If this option is used, the
                          cluster table is written to a file called
                          OUTPREF_ClstTable_rXX_aXX.1D. Otherwise the
                          table is written to stdout. 
                          You can specify the output format by adding
                          extensions to OUTPREF. For example, 
                          OUTPREF.1D.dset will force the output to be 
                          in the .1D format. 
                          See ConvertDset for many more format options.
         -out_clusterdset: Output a clustered version of inData.1D 
                           preserving only the values of nodes that 
                           belong to clusters that passed the rmm and amm2
                           conditions above.
                           The clustered dset's prefix has
                           _Clustered_rXX_aXX affixed to the OUTPREF
         -out_roidset: Output an ROI dataset with the value
                       at each node being the rank of its
                       cluster. The ROI dataset's prefix has
                       _ClstMsk_rXX_aXX affixed to the OUTPREF
                       where XX represent the values for the
                       the -rmm and -amm2 options respectively.
                       The program will not overwrite pre-existing
                       dsets.
         -prepend_node_index: Force the output dataset to have node
                        indices in column 0 of output. Use this option
                        if you are parsing .1D format datasets.
         -out_fulllist: Output a value for all nodes of insurf.
                        This option must be used in conjuction with
                        -out_roidset and/or out_clusterdset.
                        With this option, the output files might
                        be mostly 0, if you have small clusters.
                        However, you should use it if you are to 
                        maintain the same row-to-node correspondence
                        across multiple datasets.
         -sort_none: No sorting of ROI clusters.
         -sort_n_nodes: Sorting based on number of nodes
                        in cluster.
         -sort_area: Sorting based on area of clusters 
                     (default).
         -update perc: Pacify me when perc of the data have been
                       processed. perc is between 1% and 50%.
                       Default is no update.
         -no_cent: Do not find the central nodes.
                   Finding the central node is a 
                   relatively slow operation. Use
                   this option to skip it.
         -cent: Do find the central nodes (default)
    
      The cluster table output:
      A table where ach row shows results from one cluster.
      Each row contains 13 columns:   
         Col. 0  Rank of cluster (sorting order).
         Col. 1  Number of nodes in cluster.
         Col. 2  Total area of cluster. Units are the 
                 the surface coordinates' units^2.
         Col. 3  Mean data value in cluster.
         Col. 4  Mean of absolute data value in cluster.
         Col. 5  Central node of cluster (see below).
         Col. 6  Weighted central node (see below).
         Col. 7  Minimum value in cluster.
         Col. 8  Node where minimum value occurred.
         Col. 9  Maximum value in cluster.
         Col. 10 Node where maximum value occurred.
         Col. 11 Variance of values in cluster.
         Col. 12 Standard error of the mean ( sqrt(variance/number of nodes) ).
         Col. 13 = Minimum |value|
         Col. 14 = |Minimum| node
         Col. 15  = Maximum |value|
         Col. 16 = |Maximum| node
         Col. 17 = Center of Mass x
         Col. 18 = Center of Mass y
         Col. 19 = Center of Mass z
         Col. 20 = Centroid x
         Col. 21 = Centroid y
         Col. 22 = Centroid z
       The CenterNode n is such that: 
       ( sum (Uia * dia * wi) ) - ( Uca * dca * sum (wi) ) is minimal
         where i is a node in the cluster
               a is an anchor node on the surface
               sum is carried over all nodes i in a cluster
               w. is the weight of a node 
                  = 1.0 for central node 
                  = value at node for the weighted central node
               U.. is the unit vector between two nodes
               d.. is the distance between two nodes on the graph
                  (an approximation of the geodesic distance)
       If -no_cent is used, CenterNode columns are set to 0.
    
       [-novolreg]: Ignore any Rotate, Volreg, Tagalign, 
                    or WarpDrive transformations present in 
                    the Surface Volume.
       [-noxform]: Same as -novolreg
       [-setenv "'ENVname=ENVvalue'"]: Set environment variable ENVname
                    to be ENVvalue. Quotes are necessary.
                 Example: suma -setenv "'SUMA_BackgroundColor = 1 0 1'"
                    See also options -update_env, -environment, etc
                    in the output of 'suma -help'
      Common Debugging Options:
       [-trace]: Turns on In/Out debug and Memory tracing.
                 For speeding up the tracing log, I recommend 
                 you redirect stdout to a file when using this option.
                 For example, if you were running suma you would use:
                 suma -spec lh.spec -sv ... > TraceFile
                 This option replaces the old -iodbg and -memdbg.
       [-TRACE]: Turns on extreme tracing.
       [-nomall]: Turn off memory tracing.
       [-yesmall]: Turn on memory tracing (default).
      NOTE: For programs that output results to stdout
        (that is to your shell/screen), the debugging info
        might get mixed up with your results.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
    
    
    Compile Date:
       Mar  7 2018
    
