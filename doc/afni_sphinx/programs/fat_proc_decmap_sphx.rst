***************
fat_proc_decmap
***************

.. _fat_proc_decmap:

.. contents:: 
    :depth: 4 

.. code-block:: none

    -------------------------------------------------------------------------
    
      This program makes a "directionally encoded color" (DEC) map for DTI
      results.  Basically, the directionality of the tensor's major axis
      provides the color information, and the FA value weights the
      brightness (higher FA is brighter).
    
        red   :     left <-> right
        blue  : inferior <-> superior
        green : anterior <-> posterior
    
      This program uses the first eigenvector ("V1" file, from 3dDWItoDT),
      takes its absolute value and multiplies each component by the
      voxel's FA value.  That makes a 3-vector of numbers between [0,1],
      which is turned into RGB coloration.
    
      This is basically a simple wrapper script for 3dcalc and
      3dThreetoRGB.
    
      REQUIRES: AFNI.
    
      Ver. 3.1 (PA Taylor, Sep 04, 2017)
    
    -------------------------------------------------------------------------
    
      RUNNING:
    
      This script has two *required* arguments ('-in_fa ...' and '-in_v1 ...'),
      and the rest are optional:
    
        fat_proc_decmap  \
            -in_fa    IFA                            \
            -in_v1    IV1                            \
            {-mask    MASK}                          \
            -prefix   PREFIX                         \
            {-fa_thr  FFF}                           \
            {-fa_sca  SSS}                           \
            {-workdir WWW}                           \
            {-no_clean}                              \
            {-qc_prefix    QCPREF}                   \
            {-no_cmd_out}                            \
            {-no_qc_view} 
    
       where:
       -in_fa   IFA    :input FA (scalar) map.   
       -in_v1   IV1    :input first eigenvector (3-vector) map.
       -mask    MASK   :optional mask for pickout out a region;
                        otherwise, only places with FA>0 are 
                        given coloration (which just makese sense,
                        anyways, since FA>=0?).
    
       -prefix   PPP   :set prefix (and path) for output DWI data; required.
    
       -fa_thr  FFF    :for QC1 type of DEC images, use FFF to threshold
                        where DEC values are calculated (def: FFF = 0.2).
       -fa_sca  SSS    :for QC2 type of DEC images, use SSS to scale the 
                        FA weighting of what would otherwise be a 'classical'
                        DEC map (where |V1|*FA);  this is added because 
                        sometimes the DEC map can be kind of dim when 
                        weighting by FA only; instead, in this map, RGB values
                        are given by '|V1|*FA/SSS' (def:  SSS = 0.7).
    
       -no_qc_view     :by default, a set of QC snapshots are made and
                        output.  To turn these off (why?), use this
                        switch
       -qc_prefix QCP  :by default, the QC snapshots carry the same output
                        name as the final output: PREFIX_*. You
                        can change this to be QCP_*, if you want.
    
       -workdir WWW    :specify a working directory, which can be removed;
                        (default name = '__WORKING_decmap').
    
       -no_clean       :a couple temporary files are created whilst
                        making the DEC map.  This switch tells the 
                        program to *not* delete them when finishing
                        (default is to do so).  The default prefix of 
                        working dir is '__WORKING_decmap'.
    
     -qc_prefix QCPREF :can set the prefix of the QC image files separately
                        (default is 'DEC').
       -no_qc_view     :can turn off generating QC image files (why?)
       -no_cmd_out     :don't save the command line call of this program
                        and the location where it was run (otherwise, it is
                        saved by default in the ODIR/).
    
     ------------------------------------------------------------------------
    
      OUTPUTS:
    
        PREFIX_dec.nii.gz 
            a single file of type 'rgb' that AFNI knows how to 
            display with RGB coloration when viewed as underlay: 
            made by using V1 as RGB and weighting by FA values
    
        PREFIX_dec_unwt_thr.nii.gz 
            a single file of type 'rgb' that AFNI knows how to 
            display with RGB coloration when viewed as underlay: 
            made by using V1 as RGB, *not* weighting by FA, but using FA
            to threshold where DEC values are calculated (def: FA>0.2).
    
        PREFIX_dec_sca*.nii.gz 
            A similar file to PREFIX_dec.nii.gz, but additionally
            scaled by a value (such as 0.7; see "-sca_fa SSS" option
            above); this can 'brighten' the DEC map for clarity.
    
        PREFIX_dec_qc0*.png
            a set cor, axi and sag images (each a 5x3 montage) of the 
            PREFIX_dec.nii.gz data set.
    
        PREFIX_dec_unwt_thr_qc1*.png
            a set cor, axi and sag images (each a 5x3 montage) of the 
            PREFIX_dec_unwt_thr.nii.gz data set.
    
        PREFIX_dec_sca*_qc2*.png
            a set cor, axi and sag images (each a 5x3 montage) of the 
            PREFIX_dec_sca.nii.gz data set.
    
        (working directory of temp files: these can be deleted, as desired.)
    
    -------------------------------------------------------------------------
    
      EXAMPLE:
        
        fat_proc_decmap  \
            -in_fa DTI/DT_FA+orig.                    \
            -in_v1 DTI/DT_V1+orig.                    \
            -mask  mask_DWI+orig                      \
            -prefix DEC
    
